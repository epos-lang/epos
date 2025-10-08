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
	module        *ir.Module
	vars          map[string]varInfo
	functions     map[string]*ir.Func
	printf        *ir.Func
	globalFmt     *ir.Global
	strFmt        *ir.Global
	ifCounter     int
	whileCounter  int
	matchCounter  int
	stringCounter int
}

type varInfo struct {
	Alloc *ir.InstAlloca
	Typ   types.Type
}

// NewCodeGen creates a new CodeGen
func NewCodeGen() *CodeGen {
	m := ir.NewModule()

	printfTy := types.NewFunc(types.I32, types.NewPointer(types.I8))
	printfTy.Variadic = true
	printf := m.NewFunc("printf", printfTy)

	fmtStr := constant.NewCharArrayFromString("%f\n\x00")
	globalFmt := m.NewGlobalDef("fmt", fmtStr)
	strFmtStr := constant.NewCharArrayFromString("%s\n\x00")
	strFmt := m.NewGlobalDef("strfmt", strFmtStr)

	return &CodeGen{module: m, vars: make(map[string]varInfo), functions: make(map[string]*ir.Func), printf: printf, globalFmt: globalFmt, strFmt: strFmt, ifCounter: 0, whileCounter: 0, matchCounter: 0, stringCounter: 0}
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
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return types.Double
	case *parser.StringExpr:
		return types.NewPointer(types.I8)
	case *parser.MatchExpr:
		if e.Default != nil {
			return cg.getExprType(e.Default)
		} else if len(e.Cases) > 0 {
			return cg.getExprType(e.Cases[0].Body)
		}
		return types.Double // fallback to double if no cases or default
	case *parser.VarExpr:
		return types.Double // assuming variables are double
	case *parser.BinaryExpr:
		return types.Double // assuming numeric operations
	case *parser.CallExpr:
		if e.Callee == "print" {
			return types.Double
		} else if e.Callee == "elem" {
			return types.Double // assuming elem returns a number
		}
		f, ok := cg.functions[e.Callee]
		if !ok {
			panic("undefined function: " + e.Callee)
		}
		retType := f.Sig.RetType
		if types.IsPointer(retType) {
			return types.Double // Convert pointer return types to double for consistency in operations
		}
		if !retType.Equal(types.Double) {
			return types.Double // Force double for non-double return types
		}
		return retType
	case *parser.ListExpr:
		return types.NewPointer(types.Double) // lists are pointers to arrays of doubles
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
		name = "epos_user_main"
	}
	f := cg.module.NewFunc(name, rt, paramList...)
	cg.functions[s.Name] = f
	entry := f.NewBlock("entry")

	localVars := make(map[string]varInfo)
	for _, param := range f.Params {
		alloc := entry.NewAlloca(param.Type())
		entry.NewStore(param, alloc)
		localVars[param.LocalName] = varInfo{Alloc: alloc, Typ: param.Type()}
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

func (cg *CodeGen) genStmt(bb *ir.Block, stmt parser.Stmt, vars map[string]varInfo) *ir.Block {
	switch s := stmt.(type) {
	case *parser.AssignStmt:
		val, bb := cg.genExpr(bb, s.Expr, vars)
		typ := val.Type()
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
		return bb.NewLoad(v.Typ, v.Alloc), bb
	case *parser.UnaryExpr:
		if e.Op == parser.TokenMinus {
			zero := constant.NewFloat(types.Double, 0)
			operand, bb := cg.genExpr(bb, e.Expr, vars)
			return bb.NewFSub(zero, operand), bb
		}
		panic("unknown unary operator")
		return nil, nil
	case *parser.BinaryExpr:
		left, bb := cg.genExpr(bb, e.Left, vars)
		right, bb := cg.genExpr(bb, e.Right, vars)
		// Ensure operands are of type double for numeric operations
		if types.IsPointer(left.Type()) {
			left = constant.NewFloat(types.Double, 0.0)
		}
		if types.IsPointer(right.Type()) {
			right = constant.NewFloat(types.Double, 0.0)
		}
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
			panic("unknown operator")
			return nil, nil
		}
	case *parser.CallExpr:
		if e.Callee == "print" {
			if len(e.Args) != 1 {
				panic("print takes one argument")
			}
			val, bb := cg.genExpr(bb, e.Args[0], vars)
			if val.Type().Equal(types.I1) {
				val = bb.NewSelect(val, constant.NewFloat(types.Double, 1), constant.NewFloat(types.Double, 0))
			}
			var format *ir.Global
			var fmtPtr value.Value
			if val.Type().Equal(types.Double) {
				format = cg.globalFmt
				elemType := format.Type().(*types.PointerType).ElemType
				fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			} else if types.IsPointer(val.Type()) {
				// Check if it's a pointer to double (list/array)
				if ptrType, ok := val.Type().(*types.PointerType); ok && ptrType.ElemType.Equal(types.Double) {
					// Can't print lists directly, return 0 as placeholder
					return constant.NewFloat(types.Double, 0), bb
				}
				format = cg.strFmt
				elemType := format.Type().(*types.PointerType).ElemType
				fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			} else {
				// Convert unsupported types to double if possible, otherwise log error
				// Fallback to 0.0 for unsupported types
				val = constant.NewFloat(types.Double, 0.0)
				format = cg.globalFmt
				elemType := format.Type().(*types.PointerType).ElemType
				fmtPtr = bb.NewGetElementPtr(elemType, format, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			}
			fmtPtr.(*ir.InstGetElementPtr).InBounds = true
			bb.NewCall(cg.printf, fmtPtr, val)
			return constant.NewFloat(types.Double, 0), bb
		} else if e.Callee == "elem" {
			if len(e.Args) != 2 {
				panic("elem takes two arguments")
			}
			list, bb := cg.genExpr(bb, e.Args[0], vars)
			index, bb := cg.genExpr(bb, e.Args[1], vars)
			// Convert index to integer if it's a float
			indexInt := bb.NewFPToSI(index, types.I32)
			// Use only one index for GEP as per LLVM IR requirements for array access
			ptr := bb.NewGetElementPtr(types.Double, list, indexInt)
			ptr.InBounds = true
			return bb.NewLoad(types.Double, ptr), bb
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
			if !callResult.Type().Equal(types.Double) && !types.IsPointer(callResult.Type()) && !callResult.Type().Equal(types.Void) {
				// Convert non-double, non-pointer results to double for consistency
				return bb.NewUIToFP(callResult, types.Double), currentBB
			}
			return callResult, currentBB
		}
	case *parser.MatchExpr:
		cg.matchCounter++
		id := cg.matchCounter
		val, bb := cg.genExpr(bb, e.Expr, vars)
		resultType := cg.getExprType(e)
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
		arrayType := types.NewArray(uint64(len(e.Elements)), types.Double)
		alloc := bb.NewAlloca(arrayType)
		for i, elem := range e.Elements {
			elemVal, bb := cg.genExpr(bb, elem, vars)
			if types.IsPointer(elemVal.Type()) {
				elemVal = constant.NewFloat(types.Double, 0.0)
			}
			ptr := bb.NewGetElementPtr(arrayType, alloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(i)))
			ptr.InBounds = true
			bb.NewStore(elemVal, ptr)
		}
		ptr := bb.NewGetElementPtr(arrayType, alloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr, bb
	default:
		panic("unknown expression type")
		return nil, nil
	}
}
