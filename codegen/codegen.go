package codegen

import (
	"epos/parser"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// CodeGen struct
type CodeGen struct {
	module                         *ir.Module
	vars                           map[string]varInfo
	functions                      map[string]*ir.Func
	printf                         *ir.Func
	globalFmt                      *ir.Global
	strFmt                         *ir.Global
	ifCounter                      int
	whileCounter                   int
	matchCounter                   int
	stringCounter                  int
	strlen, strcpy, strcat, malloc *ir.Func
}

type varInfo struct {
	Alloc *ir.InstAlloca
	Typ   types.Type
}

func (cg *CodeGen) toLLVMType(t parser.Type) types.Type {
	switch ty := t.(type) {
	case parser.BasicType:
		if ty == "int" {
			return types.Double
		} else if ty == "string" {
			return types.NewPointer(types.I8)
		}
	case parser.ListType:
		return types.NewPointer(cg.toLLVMType(ty.Element))
	default:
		panic("unsupported type")
		return nil
	}
	return nil
}

// NewCodeGen creates a new CodeGen
func NewCodeGen() *CodeGen {
	m := ir.NewModule()

	printf := m.NewFunc("printf", types.I32, ir.NewParam("", types.NewPointer(types.I8)))
	printf.Sig.Variadic = true

	fmtStr := constant.NewCharArrayFromString("%f\n\x00")
	globalFmt := m.NewGlobalDef("fmt", fmtStr)
	strFmtStr := constant.NewCharArrayFromString("%s\n\x00")
	strFmt := m.NewGlobalDef("strfmt", strFmtStr)

	strlen := m.NewFunc("strlen", types.I64, ir.NewParam("", types.NewPointer(types.I8)))

	strcpy := m.NewFunc("strcpy", types.NewPointer(types.I8), ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))

	strcat := m.NewFunc("strcat", types.NewPointer(types.I8), ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))

	malloc := m.NewFunc("malloc", types.NewPointer(types.I8), ir.NewParam("", types.I64))

	return &CodeGen{module: m, vars: make(map[string]varInfo), functions: make(map[string]*ir.Func), printf: printf, globalFmt: globalFmt, strFmt: strFmt, ifCounter: 0, whileCounter: 0, matchCounter: 0, stringCounter: 0, strlen: strlen, strcpy: strcpy, strcat: strcat, malloc: malloc}
}

// Removed findReturnType, getExprType, getReturnType as types are now annotated

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
	for _, p := range s.Params {
		paramList = append(paramList, ir.NewParam(p.Name, cg.toLLVMType(p.Ty)))
	}
	var rt types.Type = types.Void
	if s.ReturnType != nil {
		rt = cg.toLLVMType(s.ReturnType)
	}
	name := s.Name
	if name == "main" {
		name = "epos_user_main"
	}
	f := cg.module.NewFunc(name, rt, paramList...)
	cg.functions[s.Name] = f
	entry := f.NewBlock("entry")

	localVars := make(map[string]varInfo)
	for i, param := range f.Params {
		alloc := entry.NewAlloca(param.Type())
		entry.NewStore(param, alloc)
		localVars[param.LocalName] = varInfo{Alloc: alloc, Typ: cg.toLLVMType(s.Params[i].Ty)}
	}

	current := cg.genStmts(entry, s.Body, localVars)

	if current.Term == nil {
		if rt.Equal(types.Void) {
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

func (cg *CodeGen) genStmt(bb *ir.Block, stmt parser.Stmt, vars map[string]varInfo) *ir.Block {
	switch s := stmt.(type) {
	case *parser.AssignStmt:
		// Updated for typed variables
		val, bb := cg.genExpr(bb, s.Expr, vars)
		typ := cg.toLLVMType(s.Type)
		var alloc *ir.InstAlloca
		if existing, ok := vars[s.Var]; ok {
			if !existing.Typ.Equal(typ) {
				panic("type mismatch in assignment")
			}
			alloc = existing.Alloc
		} else {
			alloc = bb.NewAlloca(typ)
			vars[s.Var] = varInfo{Alloc: alloc, Typ: typ}
		}
		bb.NewStore(val, alloc)
		return bb

	case *parser.ReturnStmt:
		val, bb := cg.genExpr(bb, s.Expr, vars)
		bb.NewRet(val)
		return bb
	case *parser.ExprStmt:
		_, bb = cg.genExpr(bb, s.Expr, vars) // Generate but ignore result
		return bb
	case *parser.IfStmt:
		cg.ifCounter++
		id := cg.ifCounter
		condVal, bb := cg.genExpr(bb, s.Cond, vars)
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

		thenBB := bb.Parent.NewBlock(fmt.Sprintf("if_then_%d", id))
		elseBB := bb.Parent.NewBlock(fmt.Sprintf("if_else_%d", id))
		mergeBB := bb.Parent.NewBlock(fmt.Sprintf("if_merge_%d", id))

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
		cg.whileCounter++
		id := cg.whileCounter
		condBB := bb.Parent.NewBlock(fmt.Sprintf("while_cond_%d", id))
		bodyBB := bb.Parent.NewBlock(fmt.Sprintf("while_body_%d", id))
		exitBB := bb.Parent.NewBlock(fmt.Sprintf("while_exit_%d", id))

		bb.NewBr(condBB)

		condVal, condBB := cg.genExpr(condBB, s.Cond, vars)
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
	case *parser.MatchStmt:
		cg.matchCounter++
		id := cg.matchCounter
		val, bb := cg.genExpr(bb, s.Expr, vars)
		mergeBB := bb.Parent.NewBlock(fmt.Sprintf("match_merge_%d", id))
		defaultBB := bb.Parent.NewBlock(fmt.Sprintf("match_default_%d", id))
		current := bb
		for i, cas := range s.Cases {
			caseBodyBB := bb.Parent.NewBlock(fmt.Sprintf("match_case_%d_%d", id, i))
			condCurrent := current
			var next *ir.Block
			if i+1 < len(s.Cases) {
				next = bb.Parent.NewBlock(fmt.Sprintf("match_next_%d_%d", id, i+1))
			} else {
				next = defaultBB
			}
			var cond value.Value
			for j, v := range cas.Values {
				vConst, condCurrent := cg.genExpr(condCurrent, v, vars)
				cmp := condCurrent.NewFCmp(enum.FPredOEQ, val, vConst)
				if j == 0 {
					cond = cmp
				} else {
					cond = condCurrent.NewOr(cond, cmp)
				}
			}
			condCurrent.NewCondBr(cond, caseBodyBB, next)
			bodyEnd := cg.genStmt(caseBodyBB, cas.Body, vars)
			if bodyEnd.Term == nil {
				bodyEnd.NewBr(mergeBB)
			}
			current = next
		}
		var defaultEnd *ir.Block
		if s.Default != nil {
			defaultEnd = cg.genStmt(defaultBB, s.Default, vars)
		} else {
			defaultEnd = defaultBB
		}
		if defaultEnd.Term == nil {
			defaultEnd.NewBr(mergeBB)
		}
		if len(s.Cases) == 0 {
			bb.NewBr(defaultBB)
		}
		return mergeBB

	default:
		panic("unknown statement type")
	}
	return nil
}

func (cg *CodeGen) genStmts(bb *ir.Block, stmts []parser.Stmt, vars map[string]varInfo) *ir.Block {
	current := bb
	for _, stmt := range stmts {
		current = cg.genStmt(current, stmt, vars)
	}
	return current
}

func (cg *CodeGen) genExpr(bb *ir.Block, expr parser.Expr, vars map[string]varInfo) (value.Value, *ir.Block) {
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return constant.NewFloat(types.Double, e.Value), bb
	case *parser.StringExpr:
		strConst := constant.NewCharArrayFromString(e.Value + "\x00")
		cg.stringCounter++
		name := fmt.Sprintf("str_%d", cg.stringCounter)
		globalStr := cg.module.NewGlobalDef(name, strConst)
		elemType := globalStr.Type().(*types.PointerType).ElemType
		ptr := bb.NewGetElementPtr(elemType, globalStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr, bb
	case *parser.VarExpr:
		v, ok := vars[e.Name]
		if !ok {
			panic("undefined variable: " + e.Name)
		}
		return bb.NewLoad(cg.toLLVMType(e.Type), v.Alloc), bb
	case *parser.UnaryExpr:
		if e.Op == parser.TokenMinus {
			zero := constant.NewFloat(types.Double, 0)
			operand, bb := cg.genExpr(bb, e.Expr, vars)
			return bb.NewFSub(zero, operand), bb
		}
		panic("unknown unary operator")
		return nil, bb
	case *parser.BinaryExpr:
		left, bb := cg.genExpr(bb, e.Left, vars)
		right, bb := cg.genExpr(bb, e.Right, vars)
		if e.Type == parser.BasicType("int") {
			switch e.Op {
			case parser.TokenPlus:
				return bb.NewFAdd(left, right), bb
			case parser.TokenMinus:
				return bb.NewFSub(left, right), bb
			case parser.TokenMul:
				return bb.NewFMul(left, right), bb
			case parser.TokenDiv:
				return bb.NewFDiv(left, right), bb
			case parser.TokenGT:
				cmp := bb.NewFCmp(enum.FPredOGT, left, right)
				return bb.NewSelect(cmp, constant.NewFloat(types.Double, 1), constant.NewFloat(types.Double, 0)), bb
			case parser.TokenLT:
				cmp := bb.NewFCmp(enum.FPredOLT, left, right)
				return bb.NewSelect(cmp, constant.NewFloat(types.Double, 1), constant.NewFloat(types.Double, 0)), bb
			case parser.TokenEQ:
				cmp := bb.NewFCmp(enum.FPredOEQ, left, right)
				return bb.NewSelect(cmp, constant.NewFloat(types.Double, 1), constant.NewFloat(types.Double, 0)), bb
			default:
				panic("unknown numeric operator")
				return nil, bb
			}
		} else if e.Type == parser.BasicType("string") && e.Op == parser.TokenPlus {
			leftLen := bb.NewCall(cg.strlen, left)
			rightLen := bb.NewCall(cg.strlen, right)
			totalLen := bb.NewAdd(leftLen, rightLen)
			alloc := bb.NewCall(cg.malloc, bb.NewAdd(totalLen, constant.NewInt(types.I64, 1)))
			bb.NewCall(cg.strcpy, alloc, left)
			bb.NewCall(cg.strcat, alloc, right)
			return alloc, bb
		} else {
			panic("unsupported binary operation")
			return nil, bb
		}
	case *parser.CallExpr:
		if e.Callee == "print" {
			if len(e.Args) != 1 {
				panic("print takes one argument")
			}
			val, bb := cg.genExpr(bb, e.Args[0], vars)
			var format *ir.Global
			var fmtPtr value.Value
			if val.Type().Equal(types.NewPointer(types.I8)) {
				format = cg.strFmt
			} else if val.Type().Equal(types.Double) {
				format = cg.globalFmt
			} else if types.IsPointer(val.Type()) {
				// list
				return constant.NewFloat(types.Double, 0), bb
			} else {
				val = constant.NewFloat(types.Double, 0.0)
				format = cg.globalFmt
			}
			elemType := format.Type().(*types.PointerType).ElemType
			fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			fmtPtr.(*ir.InstGetElementPtr).InBounds = true
			bb.NewCall(cg.printf, fmtPtr, val)
			return constant.NewFloat(types.Double, 0), bb
		} else if e.Callee == "elem" {
			if len(e.Args) != 2 {
				panic("elem takes two arguments")
			}
			list, bb := cg.genExpr(bb, e.Args[0], vars)
			index, bb := cg.genExpr(bb, e.Args[1], vars)
			indexInt := bb.NewFPToSI(index, types.I32)
			elemTy := cg.toLLVMType(e.Type)
			ptr := bb.NewGetElementPtr(elemTy, list, indexInt)
			ptr.InBounds = true
			return bb.NewLoad(elemTy, ptr), bb
		} else {
			f, ok := cg.functions[e.Callee]
			if !ok {
				panic("undefined function: " + e.Callee)
			}
			var args []value.Value
			currentBB := bb
			for _, arg := range e.Args {
				var argVal value.Value
				argVal, currentBB = cg.genExpr(currentBB, arg, vars)
				args = append(args, argVal)
			}
			callResult := currentBB.NewCall(f, args...)
			return callResult, currentBB
		}

	case *parser.MatchExpr:
		cg.matchCounter++
		id := cg.matchCounter
		val, bb := cg.genExpr(bb, e.Expr, vars)
		resultType := cg.toLLVMType(e.Type)
		resultAlloc := bb.NewAlloca(resultType)
		contBB := bb.Parent.NewBlock(fmt.Sprintf("match_cont_%d", id))
		defaultBB := bb.Parent.NewBlock(fmt.Sprintf("match_default_%d", id))
		current := bb
		for i, cas := range e.Cases {
			caseBodyBB := bb.Parent.NewBlock(fmt.Sprintf("match_case_%d_%d", id, i))
			condCurrent := current
			var next *ir.Block
			if i+1 < len(e.Cases) {
				next = bb.Parent.NewBlock(fmt.Sprintf("match_next_%d_%d", id, i+1))
			} else {
				next = defaultBB
			}
			var cond value.Value
			for j, v := range cas.Values {
				vConst, condCurrent := cg.genExpr(condCurrent, v, vars)
				cmp := condCurrent.NewFCmp(enum.FPredOEQ, val, vConst)
				if j == 0 {
					cond = cmp
				} else {
					cond = condCurrent.NewOr(cond, cmp)
				}
			}
			condCurrent.NewCondBr(cond, caseBodyBB, next)
			bodyVal, bodyEnd := cg.genExpr(caseBodyBB, cas.Body, vars)
			if bodyEnd.Term != nil {
				panic("control flow in match case body")
			} else if bodyEnd == nil {
				bodyEnd = caseBodyBB
			}
			bodyEnd.NewStore(bodyVal, resultAlloc)
			bodyEnd.NewBr(contBB)
			current = next
		}
		var defaultVal value.Value
		var defaultEnd *ir.Block
		if e.Default != nil {
			defaultVal, defaultEnd = cg.genExpr(defaultBB, e.Default, vars)
			if defaultEnd.Term != nil {
				panic("control flow in match default body")
			} else if defaultEnd == nil {
				defaultEnd = defaultBB
			}
			defaultEnd.NewStore(defaultVal, resultAlloc)
		} else {
			defaultEnd = defaultBB
		}
		if defaultEnd.Term == nil {
			defaultEnd.NewBr(contBB)
		} else {
			defaultEnd.NewBr(contBB)
		}
		if len(e.Cases) == 0 {
			bb.NewBr(defaultBB)
		}
		postBB := contBB.Parent.NewBlock(fmt.Sprintf("match_post_%d", id))
		contBB.NewBr(postBB)
		load := postBB.NewLoad(resultType, resultAlloc)
		return load, postBB
	case *parser.ListExpr:
		elemTy := cg.toLLVMType(e.Type.(parser.ListType).Element)
		arrayType := types.NewArray(uint64(len(e.Elements)), elemTy)
		alloc := bb.NewAlloca(arrayType)
		for i, elem := range e.Elements {
			elemVal, bb := cg.genExpr(bb, elem, vars)
			ptr := bb.NewGetElementPtr(arrayType, alloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(i)))
			ptr.InBounds = true
			bb.NewStore(elemVal, ptr)
		}
		ptr := bb.NewGetElementPtr(arrayType, alloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr, bb
	default:
		panic("unknown expression type")
		return nil, bb
	}
}
