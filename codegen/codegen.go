package codegen

import (
	"lua_llvm/parser"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// CodeGen struct
type CodeGen struct {
	module    *ir.Module
	vars      map[string]*ir.InstAlloca
	functions map[string]*ir.Func
	printf    *ir.Func
	globalFmt *ir.Global
}

// NewCodeGen creates a new CodeGen
func NewCodeGen() *CodeGen {
	m := ir.NewModule()

	printfTy := types.NewFunc(types.I32, types.NewPointer(types.I8))
	printfTy.Variadic = true
	printf := m.NewFunc("printf", printfTy)

	fmtStr := constant.NewCharArrayFromString("%f\n\x00")
	globalFmt := m.NewGlobalDef("fmt", fmtStr)

	return &CodeGen{module: m, vars: make(map[string]*ir.InstAlloca), functions: make(map[string]*ir.Func), printf: printf, globalFmt: globalFmt}
}

func (cg *CodeGen) findReturnType(stmts []parser.Stmt) types.Type {
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *parser.ReturnStmt:
			return cg.getExprType(s.Expr)
		case *parser.IfStmt:
			rt := cg.findReturnType(s.Then)
			if !rt.Equal(types.Void) {
				return rt
			}
			rt = cg.findReturnType(s.Else)
			if !rt.Equal(types.Void) {
				return rt
			}
		case *parser.WhileStmt:
			rt := cg.findReturnType(s.Body)
			if !rt.Equal(types.Void) {
				return rt
			}
		}
	}
	return types.Void
}

func (cg *CodeGen) getExprType(expr parser.Expr) types.Type {
	switch expr.(type) {
	case *parser.NumberExpr:
		return types.Double
	case *parser.StringExpr:
		return types.NewPointer(types.I8)
	case *parser.VarExpr:
		return types.Double // assuming variables are double
	case *parser.BinaryExpr:
		return types.Double // assuming numeric operations
	case *parser.CallExpr:
		if ce, ok := expr.(*parser.CallExpr); ok {
			f, ok := cg.functions[ce.Callee]
			if !ok {
				panic("undefined function: " + ce.Callee)
			}
			return f.Sig.RetType
		}
	}
	panic("unknown expression type")
}

func (cg *CodeGen) getReturnType(body []parser.Stmt) types.Type {
	return cg.findReturnType(body)
}

// Generate generates LLVM IR for the given statements
func (cg *CodeGen) Generate(stmts []parser.Stmt) *ir.Module {
	main := cg.module.NewFunc("main", types.I32)
	entry := main.NewBlock("entry")

	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *parser.FunctionStmt:
			cg.genFunction(s)
		default:
			entry = cg.genStmt(entry, stmt, cg.vars)
		}
	}
	if f, ok := cg.functions["main"]; ok && len(f.Params) == 0 {
		entry.NewCall(f)
	}
	entry.NewRet(constant.NewInt(types.I32, 0))

	return cg.module
}

func (cg *CodeGen) genFunction(s *parser.FunctionStmt) {
	var paramList []*ir.Param
	for _, paramName := range s.Params {
		paramList = append(paramList, ir.NewParam(paramName, types.Double))
	}
	rt := cg.getReturnType(s.Body)
	name := s.Name
	if name == "main" {
		name = "lua_user_main"
	}
	f := cg.module.NewFunc(name, rt, paramList...)
	cg.functions[s.Name] = f
	entry := f.NewBlock("entry")

	localVars := make(map[string]*ir.InstAlloca)
	for _, param := range f.Params {
		alloc := entry.NewAlloca(types.Double)
		alloc.Align = 8
		entry.NewStore(param, alloc)
		localVars[param.LocalName] = alloc
	}

	current := cg.genStmts(entry, s.Body, localVars)

	if current.Term == nil {
		if types.IsVoid(rt) {
			current.NewRet(nil)
		} else {
			var zeroVal value.Value
			switch typ := rt.(type) {
			case *types.FloatType:
				if typ.Kind == types.FloatKindDouble {
					zeroVal = constant.NewFloat(types.Double, 0)
				}
			case *types.PointerType:
				zeroVal = constant.NewNull(typ)
			default:
				panic("unsupported return type for default return")
			}
			current.NewRet(zeroVal)
		}
	}
}

func (cg *CodeGen) genStmt(bb *ir.Block, stmt parser.Stmt, vars map[string]*ir.InstAlloca) *ir.Block {
	switch s := stmt.(type) {
	case *parser.AssignStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		var alloc *ir.InstAlloca
		if a, ok := vars[s.Var]; ok {
			alloc = a
		} else {
			alloc = bb.NewAlloca(types.Double)
			alloc.Align = 8
			vars[s.Var] = alloc
		}
		bb.NewStore(val, alloc)
		return bb
	case *parser.PrintStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		var format *ir.Global
		var fmtPtr value.Value
		if val.Type().Equal(types.Double) {
			format = cg.globalFmt
			elemType := format.Type().(*types.PointerType).ElemType
			fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		} else if types.IsPointer(val.Type()) {
			strFmtStr := constant.NewCharArrayFromString("%s\n\x00")
			format = cg.module.NewGlobalDef("strfmt", strFmtStr)
			elemType := format.Type().(*types.PointerType).ElemType
			fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		} else {
			panic("unsupported print type")
		}
		fmtPtr.(*ir.InstGetElementPtr).InBounds = true
		bb.NewCall(cg.printf, fmtPtr, val)
		return bb
	case *parser.ReturnStmt:
		val := cg.genExpr(bb, s.Expr, vars)
		bb.NewRet(val)
		return bb
	case *parser.ExprStmt:
		cg.genExpr(bb, s.Expr, vars) // Generate but ignore result
		return bb
	case *parser.IfStmt:
		condVal := cg.genExpr(bb, s.Cond, vars)
		condTyp := condVal.Type()
		var cond value.Value
		if condTyp.Equal(types.I1) {
			cond = condVal
		} else if condTyp.Equal(types.Double) {
			zero := constant.NewFloat(types.Double, 0)
			cond = bb.NewFCmp(enum.FPredONE, condVal, zero)
		} else {
			panic("invalid condition type")
		}

		thenBB := bb.Parent.NewBlock("then")
		elseBB := bb.Parent.NewBlock("else")
		mergeBB := bb.Parent.NewBlock("merge")

		bb.NewCondBr(cond, thenBB, elseBB)

		thenEnd := cg.genStmts(thenBB, s.Then, vars)
		if thenEnd.Term == nil {
			thenEnd.NewBr(mergeBB)
		}

		elseEnd := cg.genStmts(elseBB, s.Else, vars)
		if elseEnd.Term == nil {
			elseEnd.NewBr(mergeBB)
		}

		return mergeBB
	case *parser.WhileStmt:
		condBB := bb.Parent.NewBlock("cond")
		bodyBB := bb.Parent.NewBlock("body")
		exitBB := bb.Parent.NewBlock("exit")

		bb.NewBr(condBB)

		condVal := cg.genExpr(condBB, s.Cond, vars)
		condTyp := condVal.Type()
		var cond value.Value
		if condTyp.Equal(types.I1) {
			cond = condVal
		} else if condTyp.Equal(types.Double) {
			zero := constant.NewFloat(types.Double, 0)
			cond = condBB.NewFCmp(enum.FPredONE, condVal, zero)
		} else {
			panic("invalid condition type")
		}

		condBB.NewCondBr(cond, bodyBB, exitBB)

		bodyEnd := cg.genStmts(bodyBB, s.Body, vars)
		if bodyEnd.Term == nil {
			bodyEnd.NewBr(condBB)
		}

		return exitBB
	default:
		panic("unknown statement type")
	}
}

func (cg *CodeGen) genStmts(bb *ir.Block, stmts []parser.Stmt, vars map[string]*ir.InstAlloca) *ir.Block {
	current := bb
	for _, stmt := range stmts {
		current = cg.genStmt(current, stmt, vars)
	}
	return current
}

func (cg *CodeGen) genExpr(bb *ir.Block, expr parser.Expr, vars map[string]*ir.InstAlloca) value.Value {
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return constant.NewFloat(types.Double, e.Value)
	case *parser.StringExpr:
		strConst := constant.NewCharArrayFromString(e.Value + "\x00")
		globalStr := cg.module.NewGlobalDef("", strConst)
		elemType := globalStr.Type().(*types.PointerType).ElemType
		ptr := bb.NewGetElementPtr(elemType, globalStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr
	case *parser.VarExpr:
		alloc, ok := vars[e.Name]
		if !ok {
			panic("undefined variable: " + e.Name)
		}
		return bb.NewLoad(types.Double, alloc)
	case *parser.UnaryExpr:
		if e.Op == parser.TokenMinus {
			zero := constant.NewFloat(types.Double, 0)
			operand := cg.genExpr(bb, e.Expr, vars)
			return bb.NewFSub(zero, operand)
		}
		panic("unknown unary operator")
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
		case parser.TokenGT:
			return bb.NewFCmp(enum.FPredOGT, left, right)
		case parser.TokenLT:
			return bb.NewFCmp(enum.FPredOLT, left, right)
		case parser.TokenEQ:
			return bb.NewFCmp(enum.FPredOEQ, left, right)
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
