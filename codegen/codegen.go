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
	listPrintCounter               int
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
			return types.I64
		} else if ty == "string" {
			return types.NewPointer(types.I8)
		}
	case parser.ListType:
		s := types.NewStruct(types.I64, types.NewPointer(cg.toLLVMType(ty.Element)))
		return types.NewPointer(s)
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

	return &CodeGen{module: m, vars: make(map[string]varInfo), functions: make(map[string]*ir.Func), printf: printf, globalFmt: globalFmt, strFmt: strFmt, ifCounter: 0, whileCounter: 0, matchCounter: 0, stringCounter: 0, listPrintCounter: 0, strlen: strlen, strcpy: strcpy, strcat: strcat, malloc: malloc}
}

func (cg *CodeGen) getParserType(expr parser.Expr) parser.Type {
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return e.Type
	case *parser.BoolExpr:
		return e.Type
	case *parser.StringExpr:
		return e.Type
	case *parser.VarExpr:
		return e.Type
	case *parser.BinaryExpr:
		return e.Type
	case *parser.UnaryExpr:
		return e.Type
	case *parser.CallExpr:
		return e.Type
	case *parser.MatchExpr:
		return e.Type
	case *parser.ListExpr:
		return e.Type
	default:
		panic("unknown expr type for getParserType")
	}
	return nil
}

func (cg *CodeGen) printString(bb *ir.Block, s string) *ir.Block {
	strConst := constant.NewCharArrayFromString(s + "\x00")
	cg.stringCounter++
	global := cg.module.NewGlobalDef(fmt.Sprintf("print_str_%d", cg.stringCounter), strConst)
	elemTy := global.Type().(*types.PointerType).ElemType
	ptr := bb.NewGetElementPtr(elemTy, global, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	ptr.InBounds = true
	bb.NewCall(cg.printf, ptr)
	return bb
}

func (cg *CodeGen) genPrint(bb *ir.Block, val value.Value, pty parser.Type, vars map[string]varInfo, addNewline bool) *ir.Block {
	nl := ""
	if addNewline {
		nl = "\n"
	}
	switch ty := pty.(type) {
	case parser.BasicType:
		switch string(ty) {
		case "int":
			intFmt := cg.module.NewGlobalDef(fmt.Sprintf("int_fmt_%d", cg.stringCounter), constant.NewCharArrayFromString("%lld"+nl+"\x00"))
			cg.stringCounter++
			fmtPtr := bb.NewGetElementPtr(intFmt.Type().(*types.PointerType).ElemType, intFmt, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			fmtPtr.InBounds = true
			bb.NewCall(cg.printf, fmtPtr, val)
		case "bool":
			trueStr := cg.module.NewGlobalDef(fmt.Sprintf("true_str_%d", cg.stringCounter), constant.NewCharArrayFromString("true"+nl+"\x00"))
			cg.stringCounter++
			falseStr := cg.module.NewGlobalDef(fmt.Sprintf("false_str_%d", cg.stringCounter), constant.NewCharArrayFromString("false"+nl+"\x00"))
			cg.stringCounter++
			truePtr := bb.NewGetElementPtr(trueStr.Type().(*types.PointerType).ElemType, trueStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			truePtr.InBounds = true
			falsePtr := bb.NewGetElementPtr(falseStr.Type().(*types.PointerType).ElemType, falseStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			falsePtr.InBounds = true
			selected := bb.NewSelect(val, truePtr, falsePtr)
			bb.NewCall(cg.printf, selected)
		case "string":
			strFmt := cg.module.NewGlobalDef(fmt.Sprintf("str_fmt_%d", cg.stringCounter), constant.NewCharArrayFromString("%s"+nl+"\x00"))
			cg.stringCounter++
			fmtPtr := bb.NewGetElementPtr(strFmt.Type().(*types.PointerType).ElemType, strFmt, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			fmtPtr.InBounds = true
			bb.NewCall(cg.printf, fmtPtr, val)
		}
	case parser.ListType:
		bb = cg.printString(bb, "[")
		structPtr := val
		structTy := structPtr.Type().(*types.PointerType).ElemType
		lengthPtr := bb.NewGetElementPtr(structTy, structPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		length := bb.NewLoad(types.I64, lengthPtr)
		dataPtrPtr := bb.NewGetElementPtr(structTy, structPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
		dataPtr := bb.NewLoad(dataPtrPtr.Type().(*types.PointerType).ElemType, dataPtrPtr)
		zero := constant.NewInt(types.I64, 0)
		one := constant.NewInt(types.I64, 1)
		isZero := bb.NewICmp(enum.IPredEQ, length, zero)
		cg.listPrintCounter++
		id := cg.listPrintCounter
		zeroBB := bb.Parent.NewBlock(fmt.Sprintf("list_zero_%d", id))
		loopCondBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_cond_%d", id))
		loopBodyBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_body_%d", id))
		loopIncBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_inc_%d", id))
		afterBB := bb.Parent.NewBlock(fmt.Sprintf("list_after_%d", id))
		bb.NewCondBr(isZero, zeroBB, loopCondBB)
		zeroBB.NewBr(afterBB)

		i := loopCondBB.NewPhi(ir.NewIncoming(zero, bb))
		cmp := loopCondBB.NewICmp(enum.IPredSLT, i, length)
		loopCondBB.NewCondBr(cmp, loopBodyBB, afterBB)
		isFirst := loopBodyBB.NewICmp(enum.IPredEQ, i, zero)
		firstBB := loopBodyBB.Parent.NewBlock(fmt.Sprintf("list_first_%d", id))
		commaBB := loopBodyBB.Parent.NewBlock(fmt.Sprintf("list_comma_%d", id))
		mergePrintBB := loopBodyBB.Parent.NewBlock(fmt.Sprintf("list_merge_print_%d", id))
		loopBodyBB.NewCondBr(isFirst, firstBB, commaBB)
		commaBB = cg.printString(commaBB, ", ")
		commaBB.NewBr(mergePrintBB)
		firstBB.NewBr(mergePrintBB)
		elemTy := cg.toLLVMType(ty.Element)
		elemPtr := mergePrintBB.NewGetElementPtr(elemTy, dataPtr, i)
		elemPtr.InBounds = true
		elem := mergePrintBB.NewLoad(elemTy, elemPtr)
		switch elTy := ty.Element.(type) {
		case parser.BasicType:
			if string(elTy) == "string" {
				mergePrintBB = cg.printString(mergePrintBB, "\"")
			}
			mergePrintBB = cg.genPrint(mergePrintBB, elem, ty.Element, vars, false)
			if string(elTy) == "string" {
				mergePrintBB = cg.printString(mergePrintBB, "\"")
			}
		default:
			mergePrintBB = cg.genPrint(mergePrintBB, elem, ty.Element, vars, false)
		}
		mergePrintBB.NewBr(loopIncBB)
		incI := loopIncBB.NewAdd(i, one)
		loopIncBB.NewBr(loopCondBB)
		i.Incs = append(i.Incs, ir.NewIncoming(incI, loopIncBB))
		s := "]"
		if addNewline {
			s += "\n"
		}
		afterBB = cg.printString(afterBB, s)
		return afterBB
	}
	return bb
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
			case *types.IntType:
				zeroVal = constant.NewInt(typ, 0)
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
		} else if condTyp.Equal(types.I64) {
			zero := constant.NewInt(types.I64, 0)
			cond = bb.NewICmp(enum.IPredNE, condVal, zero)
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
				cmp := condCurrent.NewICmp(enum.IPredEQ, val, vConst)
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
		return constant.NewInt(types.I64, e.Value), bb
	case *parser.BoolExpr:
		return constant.NewBool(e.Value), bb
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
			zero := constant.NewInt(types.I64, 0)
			operand, bb := cg.genExpr(bb, e.Expr, vars)
			return bb.NewSub(zero, operand), bb
		}
		panic("unknown unary operator")
		return nil, bb
	case *parser.BinaryExpr:
		left, bb := cg.genExpr(bb, e.Left, vars)
		right, bb := cg.genExpr(bb, e.Right, vars)
		// leftTy := left.Type()
		if e.Type == parser.BasicType("int") {
			switch e.Op {
			case parser.TokenPlus:
				return bb.NewAdd(left, right), bb
			case parser.TokenMinus:
				return bb.NewSub(left, right), bb
			case parser.TokenMul:
				return bb.NewMul(left, right), bb
			case parser.TokenDiv:
				return bb.NewSDiv(left, right), bb
			default:
				panic("unknown integer operator")
			}
		} else if e.Type == parser.BasicType("bool") {
			var pred enum.IPred
			switch e.Op {
			case parser.TokenGT:
				pred = enum.IPredSGT
			case parser.TokenLT:
				pred = enum.IPredSLT
			case parser.TokenEQ:
				pred = enum.IPredEQ
			}
			return bb.NewICmp(pred, left, right), bb
		} else if e.Type == parser.BasicType("string") && e.Op == parser.TokenPlus {
			leftLen := bb.NewCall(cg.strlen, left)
			rightLen := bb.NewCall(cg.strlen, right)
			totalLen := bb.NewAdd(leftLen, rightLen)
			alloc := bb.NewCall(cg.malloc, bb.NewAdd(totalLen, constant.NewInt(types.I32, 1)))
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
			pty := cg.getParserType(e.Args[0])
			bb = cg.genPrint(bb, val, pty, vars, true)
			return constant.NewInt(types.I32, 0), bb
		} else if e.Callee == "elem" {
			if len(e.Args) != 2 {
				panic("elem takes two arguments")
			}
			listPtr, bb := cg.genExpr(bb, e.Args[0], vars)
			index, bb := cg.genExpr(bb, e.Args[1], vars)
			structTy := listPtr.Type().(*types.PointerType).ElemType
			//	lengthPtr := bb.NewGetElementPtr(structTy, listPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			// length := bb.NewLoad(types.I64, lengthPtr)
			dataPtrPtr := bb.NewGetElementPtr(structTy, listPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			dataPtr := bb.NewLoad(dataPtrPtr.Type().(*types.PointerType).ElemType, dataPtrPtr)
			elemTy := cg.toLLVMType(e.Type)
			elemPtr := bb.NewGetElementPtr(elemTy, dataPtr, index)
			elemPtr.InBounds = true
			return bb.NewLoad(elemTy, elemPtr), bb
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
				cmp := condCurrent.NewICmp(enum.IPredEQ, val, vConst)
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
		if len(e.Elements) == 0 {
			return constant.NewNull(cg.toLLVMType(e.Type).(*types.PointerType)), bb
		}
		arrayTy := types.NewArray(uint64(len(e.Elements)), elemTy)
		arrayAlloc := bb.NewAlloca(arrayTy)
		for i, el := range e.Elements {
			val, bb := cg.genExpr(bb, el, vars)
			ptr := bb.NewGetElementPtr(arrayTy, arrayAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I64, int64(i)))
			ptr.InBounds = true
			bb.NewStore(val, ptr)
		}
		dataPtr := bb.NewGetElementPtr(arrayTy, arrayAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		dataPtr.InBounds = true
		structTy := types.NewStruct(types.I64, types.NewPointer(elemTy))
		structAlloc := bb.NewAlloca(structTy)
		lengthPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		bb.NewStore(constant.NewInt(types.I64, int64(len(e.Elements))), lengthPtr)
		dataFieldPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
		bb.NewStore(dataPtr, dataFieldPtr)
		return structAlloc, bb
	default:
		panic("unknown expression type")
		return nil, bb
	}
}
