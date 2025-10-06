package codegen

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"

	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
	"lua_llvm/parser"
)

// CodeGen struct
type CodeGen struct {
	module    *ir.Module
	vars      map[string]value.Value
	functions map[string]*ir.Func
}

// NewCodeGen creates a new CodeGen
func NewCodeGen() *CodeGen {
	m := ir.NewModule()
	return &CodeGen{module: m, vars: make(map[string]value.Value), functions: make(map[string]*ir.Func)}
}

// Generate generates LLVM IR for the given statements
func (cg *CodeGen) Generate(stmts []parser.Stmt) *ir.Module {
	main := cg.module.NewFunc("main", types.I32)
	entry := main.NewBlock("entry")

	// Declare printf
	printfTy := types.NewFunc(types.I32, types.NewPointer(types.I8))
	printfTy.Variadic = true
	printf := cg.module.NewFunc("printf", printfTy)
	// printf.Linkage = enum.LinkageExternal

	fmtStr := constant.NewCharArrayFromString("%f\n\x00")
	globalFmt := cg.module.NewGlobalDef("fmt", fmtStr)

	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *parser.FunctionStmt:
			cg.genFunction(s)
		default:
			cg.genStmt(entry, stmt, cg.vars)
		}
	}
	entry.NewRet(constant.NewInt(types.I32, 0))

	return cg.module
}

func (cg *CodeGen) genFunction(s *parser.FunctionStmt) {
	paramsTy := []types.Type{}
	for range s.Params {
		paramsTy = append(paramsTy, types.Double)
	}
	funcTy := types.NewFunc(types.Double, paramsTy...)
	f := cg.module.NewFunc(s.Name, funcTy)
	entry := f.NewBlock("entry")

	localVars := make(map[string]value.Value)
	for i, paramName := range s.Params {
		param := f.Params[i]
		alloc := entry.NewAlloca(types.Double)
		entry.NewStore(param, alloc)
		localVars[paramName] = alloc
	}

	for _, stmt := range s.Body {
		cg.genStmt(entry, stmt, localVars)
	}

	entry.NewRet(constant.NewFloat(types.Double, 0))
	cg.functions[s.Name] = f
}

func (cg *CodeGen) genStmt(bb *ir.Block, stmt parser.Stmt, vars map[string]value.Value) {
	switch s := stmt.(type) {
	case *parser.AssignStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		alloc := bb.NewAlloca(types.Double)
		bb.NewStore(val, alloc)
		vars[s.Var] = alloc
	case *parser.PrintStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		ptr := bb.NewGetElementPtr(types.NewArray(4, types.I8), globalFmt, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		bb.NewCall(printf, ptr, val)
	case *parser.ReturnStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		bb.NewRet(val)
	default:
		panic("unknown statement type")
	}
}

func (cg *CodeGen) genExpr(bb *ir.Block, expr parser.Expr, vars map[string]value.Value) value.Value {
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return constant.NewFloat(types.Double, e.Value)
	case *parser.VarExpr:
		alloc, ok := vars[e.Name]
		if !ok {
			panic("undefined variable: " + e.Name)
		}
		return bb.NewLoad(types.Double, alloc)
	case *parser.BinaryExpr:
		left := cg.genExpr(bb, e.Left, vars)
		right := cg.genExpr(bb, e.Right, vars)
		switch e.Op {
		case parser.TokenPlus:
			return bb.NewFAdd(left, right)
		case parser.TokenMinus:
			return bb.NewFSub(left, right)
		case parser.TokenMul:
			return bb.NewFMul(left, right)
		case parser.TokenDiv:
			return bb.NewFDiv(left, right)
		default:
			panic("unknown operator")
		}
	case *parser.CallExpr:
		f, ok := cg.functions[e.Callee]
		if !ok {
			panic("undefined function: " + e.Callee)
		}
		var args []value.Value
		for _, arg := range e.Args {
			args = append(args, cg.genExpr(bb, arg, vars))
		}
		return bb.NewCall(f, args...)
	default:
		panic("unknown expression type")
	}
}
