package codegen

import (
	"epos/parser"
	"fmt"
	"strconv"

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
	recordPrintCounter             int
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
		} else if ty == "string" || ty == "type" {
			return types.NewPointer(types.I8)
		} else if ty == "bool" {
			return types.I1
		}
	case parser.FunctionType:
		var paramTypes []types.Type
		for _, p := range ty.Params {
			paramTypes = append(paramTypes, cg.toLLVMType(p))
		}
		funcTy := types.NewFunc(cg.toLLVMType(ty.Return), paramTypes...)
		return types.NewPointer(funcTy)
	case parser.ListType:
		s := types.NewStruct(types.I64, types.NewPointer(cg.toLLVMType(ty.Element)))
		return types.NewPointer(s)
	case parser.RecordType:
		var fieldTypes []types.Type
		for _, f := range ty.Fields {
			ft := cg.toLLVMType(f.Ty)
			if f.Optional {
				ft = types.NewPointer(ft)
			}
			fieldTypes = append(fieldTypes, ft)
		}
		return types.NewPointer(types.NewStruct(fieldTypes...))
	default:
		panic("unsupported type")
		return nil
	}
	return nil
}

func typeToString(t parser.Type) string {
	switch ty := t.(type) {
	case parser.BasicType:
		return string(ty)
	case parser.ListType:
		return "list(" + typeToString(ty.Element) + ")"
	case parser.RecordType:
		s := "@{"
		for i, f := range ty.Fields {
			if i > 0 {
				s += ", "
			}
			s += f.Name + ": " + typeToString(f.Ty)
			if f.Optional {
				s += "?"
			}
		}
		s += "}"
		return s
	case parser.FunctionType:
		s := "fn("
		for i, p := range ty.Params {
			if i > 0 {
				s += ", "
			}
			s += typeToString(p)
		}
		s += ") -> " + typeToString(ty.Return)
		return s
	default:
		return "unknown"
	}
}

// NewCodeGen creates a new CodeGen
func NewCodeGen() *CodeGen {
	m := ir.NewModule()

	printf := m.NewFunc("printf", types.I32, ir.NewParam("", types.NewPointer(types.I8)))
	printf.Sig.Variadic = true

	fmtStr := constant.NewCharArrayFromString("%f\x00")
	globalFmt := m.NewGlobalDef("fmt", fmtStr)
	strFmtStr := constant.NewCharArrayFromString("%s\x00")
	strFmt := m.NewGlobalDef("strfmt", strFmtStr)

	strlen := m.NewFunc("strlen", types.I64, ir.NewParam("", types.NewPointer(types.I8)))

	strcpy := m.NewFunc("strcpy", types.NewPointer(types.I8), ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))

	strcat := m.NewFunc("strcat", types.NewPointer(types.I8), ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))

	malloc := m.NewFunc("malloc", types.NewPointer(types.I8), ir.NewParam("", types.I64))

	return &CodeGen{module: m, vars: make(map[string]varInfo), functions: make(map[string]*ir.Func), printf: printf, globalFmt: globalFmt, strFmt: strFmt, ifCounter: 0, whileCounter: 0, matchCounter: 0, stringCounter: 0, listPrintCounter: 0, recordPrintCounter: 0, strlen: strlen, strcpy: strcpy, strcat: strcat, malloc: malloc}
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
	case *parser.RecordExpr:
		return e.Type
	case *parser.FieldAccessExpr:
		return e.Type
	case *parser.TypeValueExpr:
		return parser.BasicType("type")
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
		case "string", "type":
			strFmt := cg.module.NewGlobalDef(fmt.Sprintf("str_fmt_%d", cg.stringCounter), constant.NewCharArrayFromString("%s"+nl+"\x00"))
			cg.stringCounter++
			fmtPtr := bb.NewGetElementPtr(strFmt.Type().(*types.PointerType).ElemType, strFmt, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			fmtPtr.InBounds = true
			bb.NewCall(cg.printf, fmtPtr, val)
		}
	case parser.ListType:
		bb = cg.printString(bb, "{")
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
		s := "}"
		if addNewline {
			s += "\n"
		}
		afterBB = cg.printString(afterBB, s)
		return afterBB
	case parser.RecordType:
		bb = cg.printString(bb, "@{")
		structPtr := val
		structTy := structPtr.Type().(*types.PointerType).ElemType
		rt := pty.(parser.RecordType)
		first := true
		cg.recordPrintCounter++
		id := cg.recordPrintCounter
		for i, f := range rt.Fields {
			fieldPtr := bb.NewGetElementPtr(structTy, structPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(i)))
			fieldPtr.InBounds = true
			fieldVal := bb.NewLoad(fieldPtr.Type().(*types.PointerType).ElemType, fieldPtr)
			skipBB := bb.Parent.NewBlock(fmt.Sprintf("skip_%d_%d", id, i))
			printBB := bb.Parent.NewBlock(fmt.Sprintf("print_%d_%d", id, i))
			if f.Optional {
				null := constant.NewNull(fieldVal.Type().(*types.PointerType))
				isNull := bb.NewICmp(enum.IPredEQ, fieldVal, null)
				bb.NewCondBr(isNull, skipBB, printBB)
			} else {
				bb.NewBr(printBB)
			}
			bb = printBB
			if !first {
				printBB = cg.printString(printBB, ", ")
			}
			first = false
			printBB = cg.printString(printBB, f.Name+" => ")
			if f.Optional {
				fieldVal = printBB.NewLoad(fieldVal.Type().(*types.PointerType).ElemType, fieldVal)
			}
			printBB = cg.genPrint(printBB, fieldVal, f.Ty, vars, false)
			printBB.NewBr(skipBB)
			bb = skipBB
		}
		bb = cg.printString(bb, "}")
		if addNewline {
			bb = cg.printString(bb, "\n")
		}
		return bb
	case parser.FunctionType:
		bb = cg.printString(bb, "<function ")
		typeStr := typeToString(pty)
		strConst := constant.NewCharArrayFromString(typeStr + ">" + nl + "\x00")
		cg.stringCounter++
		global := cg.module.NewGlobalDef(fmt.Sprintf("func_type_str_%d", cg.stringCounter), strConst)
		ptr := bb.NewGetElementPtr(global.Type().(*types.PointerType).ElemType, global, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		bb.NewCall(cg.printf, ptr)
		return bb
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
		if ok {
			return bb.NewLoad(cg.toLLVMType(e.Type), v.Alloc), bb
		}
		f, ok := cg.functions[e.Name]
		if ok {
			return f, bb
		}
		panic("undefined: " + e.Name)
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
		var callee value.Value
		if ve, ok := e.Callee.(*parser.VarExpr); ok {
			calleeName := ve.Name
			if calleeName == "print" {
				val, bb := cg.genExpr(bb, e.Args[0], vars)
				pty := cg.getParserType(e.Args[0])
				bb = cg.genPrint(bb, val, pty, vars, true)
				return constant.NewInt(types.I32, 0), bb
			} else if calleeName == "elem" {
				var listPtr, index value.Value
				listPtr, bb = cg.genExpr(bb, e.Args[0], vars)
				index, bb = cg.genExpr(bb, e.Args[1], vars)
				structTy := listPtr.Type().(*types.PointerType).ElemType
				dataPtrPtr := bb.NewGetElementPtr(structTy, listPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
				dataPtr := bb.NewLoad(dataPtrPtr.Type().(*types.PointerType).ElemType, dataPtrPtr)
				elemTy := cg.toLLVMType(e.Type)
				elemPtr := bb.NewGetElementPtr(elemTy, dataPtr, index)
				elemPtr.InBounds = true
				return bb.NewLoad(elemTy, elemPtr), bb
			} else if vi, ok := vars[calleeName]; ok {
				callee = bb.NewLoad(vi.Typ, vi.Alloc)
			} else if f, ok := cg.functions[calleeName]; ok {
				callee = f
			} else {
				panic("undefined: " + calleeName)
			}
		} else {
			callee, bb = cg.genExpr(bb, e.Callee, vars)
		}
		var args []value.Value
		for _, arg := range e.Args {
			argVal, _ := cg.genExpr(bb, arg, vars)
			// argVal, bb := cg.genExpr(bb, arg, vars)
			args = append(args, argVal)
		}
		return bb.NewCall(callee, args...), bb
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
		nullPtrArray := constant.NewNull(types.NewPointer(arrayTy))
		sizePtrArray := bb.NewGetElementPtr(arrayTy, nullPtrArray, constant.NewInt(types.I64, 1))
		sizePtrArray.InBounds = true
		sizeArray := bb.NewPtrToInt(sizePtrArray, types.I64)
		mallocedArray := bb.NewCall(cg.malloc, sizeArray)
		arrayAlloc := bb.NewBitCast(mallocedArray, types.NewPointer(arrayTy))
		for i, el := range e.Elements {
			val, bb := cg.genExpr(bb, el, vars)
			ptr := bb.NewGetElementPtr(arrayTy, arrayAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I64, int64(i)))
			ptr.InBounds = true
			bb.NewStore(val, ptr)
		}
		dataPtr := bb.NewGetElementPtr(arrayTy, arrayAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		dataPtr.InBounds = true
		structTy := types.NewStruct(types.I64, types.NewPointer(elemTy))
		nullPtrStruct := constant.NewNull(types.NewPointer(structTy))
		sizePtrStruct := bb.NewGetElementPtr(structTy, nullPtrStruct, constant.NewInt(types.I64, 1))
		sizePtrStruct.InBounds = true
		sizeStruct := bb.NewPtrToInt(sizePtrStruct, types.I64)
		mallocedStruct := bb.NewCall(cg.malloc, sizeStruct)
		structAlloc := bb.NewBitCast(mallocedStruct, types.NewPointer(structTy))
		lengthPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		bb.NewStore(constant.NewInt(types.I64, int64(len(e.Elements))), lengthPtr)
		dataFieldPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
		bb.NewStore(dataPtr, dataFieldPtr)
		return structAlloc, bb
	case *parser.RecordExpr:
		structTy := cg.toLLVMType(e.Type).(*types.PointerType).ElemType
		nullPtr := constant.NewNull(types.NewPointer(structTy))
		sizePtr := bb.NewGetElementPtr(structTy, nullPtr, constant.NewInt(types.I64, 1))
		sizePtr.InBounds = true
		size := bb.NewPtrToInt(sizePtr, types.I64)
		malloced := bb.NewCall(cg.malloc, size)
		alloc := bb.NewBitCast(malloced, types.NewPointer(structTy))
		fieldIndex := 0
		rt := e.Type.(parser.RecordType)
		for _, f := range rt.Fields {
			fieldPtr := bb.NewGetElementPtr(structTy, alloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(fieldIndex)))
			fieldPtr.InBounds = true
			fieldIndex++
			if valExpr, ok := e.Fields[f.Name]; ok {
				val, bb := cg.genExpr(bb, valExpr, vars)
				if f.Optional {
					falloc := bb.NewAlloca(val.Type())
					bb.NewStore(val, falloc)
					bb.NewStore(falloc, fieldPtr)
				} else {
					bb.NewStore(val, fieldPtr)
				}
			} else {
				if !f.Optional {
					panic("missing required field " + f.Name)
				}
				elemType := fieldPtr.Type().(*types.PointerType).ElemType
				bb.NewStore(constant.NewNull(elemType.(*types.PointerType)), fieldPtr)
			}
		}
		return alloc, bb
	case *parser.FieldAccessExpr:
		rec, bb := cg.genExpr(bb, e.Receiver, vars)
		recParserTy := cg.getParserType(e.Receiver)
		rt := recParserTy.(parser.RecordType)
		fieldIndex := -1
		for i, f := range rt.Fields {
			if f.Name == e.Field {
				fieldIndex = i
				break
			}
		}
		if fieldIndex == -1 {
			panic("field not found: " + e.Field)
		}
		structTy := rec.Type().(*types.PointerType).ElemType
		fieldPtr := bb.NewGetElementPtr(structTy, rec, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(fieldIndex)))
		fieldPtr.InBounds = true
		val := bb.NewLoad(fieldPtr.Type().(*types.PointerType).ElemType, fieldPtr)
		if rt.Fields[fieldIndex].Optional {
			null := constant.NewNull(val.Type().(*types.PointerType))
			isNull := bb.NewICmp(enum.IPredEQ, val, null)
			nullBB := bb.Parent.NewBlock("null_field_" + strconv.Itoa(fieldIndex))
			loadBB := bb.Parent.NewBlock("load_field_" + strconv.Itoa(fieldIndex))
			mergeBB := bb.Parent.NewBlock("merge_field_" + strconv.Itoa(fieldIndex))
			bb.NewCondBr(isNull, nullBB, loadBB)
			var zeroVal value.Value
			switch typ := cg.toLLVMType(e.Type).(type) {
			case *types.IntType:
				if typ.BitSize == 1 {
					zeroVal = constant.NewBool(false)
				} else {
					zeroVal = constant.NewInt(typ, 0)
				}
			case *types.PointerType:
				zeroVal = constant.NewNull(typ)
			default:
				panic("unsupported type for zero value")
			}
			nullBB.NewBr(mergeBB)
			loaded := loadBB.NewLoad(val.Type().(*types.PointerType).ElemType, val)
			loadBB.NewBr(mergeBB)
			phi := mergeBB.NewPhi(ir.NewIncoming(zeroVal, nullBB), ir.NewIncoming(loaded, loadBB))
			return phi, mergeBB
		}
		return val, bb
	case *parser.TypeValueExpr:
		str := typeToString(e.Ty)
		strConst := constant.NewCharArrayFromString(str + "\x00")
		cg.stringCounter++
		name := fmt.Sprintf("type_str_%d", cg.stringCounter)
		globalStr := cg.module.NewGlobalDef(name, strConst)
		elemType := globalStr.Type().(*types.PointerType).ElemType
		ptr := bb.NewGetElementPtr(elemType, globalStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr, bb
	default:
		panic("unknown expression type")
		return nil, bb
	}
}
