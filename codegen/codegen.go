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
	module                                 *ir.Module
	vars                                   map[string]varInfo
	functions                              map[string]*ir.Func
	genericFunctions                       map[string]*parser.FunctionStmt
	printf                                 *ir.Func
	globalFmt                              *ir.Global
	strFmt                                 *ir.Global
	ifCounter                              int
	whileCounter                           int
	matchCounter                           int
	stringCounter                          int
	assertCounter                          int
	listPrintCounter                       int
	recordPrintCounter                     int
	strlen, strcpy, strcat, malloc, memcpy, strcmp, memcmp, sprintf, exit *ir.Func
	intFmt                                 *ir.Global
	intToStringCounter                     int
	assertFailMsg                          *ir.Global
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
	case parser.GenericType:
		// Generic types should have been resolved during type checking
		panic(fmt.Sprintf("unresolved generic type: %s", ty.Name))
	default:
		panic("unsupported type")
		return nil
	}
	return nil
}

func (cg *CodeGen) hasGenericType(t parser.Type) bool {
	switch typ := t.(type) {
	case parser.GenericType:
		return true
	case parser.ListType:
		return cg.hasGenericType(typ.Element)
	case parser.FunctionType:
		for _, param := range typ.Params {
			if cg.hasGenericType(param) {
				return true
			}
		}
		return cg.hasGenericType(typ.Return)
	case parser.RecordType:
		for _, field := range typ.Fields {
			if cg.hasGenericType(field.Ty) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// instantiateGenericFunction creates a specialized version of a generic function
func (cg *CodeGen) instantiateGenericFunction(name string, argTypes []parser.Type) *ir.Func {
	genericFunc, ok := cg.genericFunctions[name]
	if !ok {
	
		return nil
	}

	
	// Create type mapping from generic parameters to concrete types
	typeMap := make(map[string]parser.Type)
	for i, param := range genericFunc.Params {
		if i < len(argTypes) {
	
			cg.mapGenericTypes(param.Ty, argTypes[i], typeMap)
		}
	}
	

	
	// Also map return type generics if return type is used in body
	if cg.hasGenericType(genericFunc.ReturnType) {
		// For functions like create-list where return type is list(t),
		// we need to infer 't' from the return type of expressions in the body
		if len(genericFunc.Body) > 0 {
			if exprStmt, ok := genericFunc.Body[0].(*parser.ExprStmt); ok {
				if listExpr, ok := exprStmt.Expr.(*parser.ListExpr); ok && len(listExpr.Elements) > 0 {
					// For create-list function, the element type should already be in typeMap
					// from parameter mapping. Skip this complex inference for now.
				}
			}
		}
	}
	
	// Generate specialized function name
	specializedName := name
	for _, concreteType := range typeMap {
		specializedName += "_" + cg.typeToString(concreteType)
	}
	
	// Check if already instantiated
	if f, exists := cg.functions[specializedName]; exists {
		return f
	}
	
	// Create specialized function
	specialized := &parser.FunctionStmt{
		Name:       specializedName,
		Params:     make([]parser.Param, len(genericFunc.Params)),
		ReturnType: cg.substituteGenericType(genericFunc.ReturnType, typeMap),
		Body:       cg.substituteGenericTypesInStmts(genericFunc.Body, typeMap),
	}
	
	// Substitute generic types in parameters
	for i, param := range genericFunc.Params {
		specialized.Params[i] = parser.Param{
			Name: param.Name,
			Ty:   cg.substituteGenericType(param.Ty, typeMap),
		}
	}
	
	// Generate the specialized function
	cg.declareFunctionSignature(specialized)
	cg.genFunctionBody(specialized)
	
	return cg.functions[specializedName]
}

// mapGenericTypes recursively maps generic type names to concrete types
func (cg *CodeGen) mapGenericTypes(generic, concrete parser.Type, typeMap map[string]parser.Type) {

	switch g := generic.(type) {
	case parser.GenericType:

		typeMap[g.Name] = concrete
	case parser.ListType:
		if c, ok := concrete.(parser.ListType); ok {
			cg.mapGenericTypes(g.Element, c.Element, typeMap)
		}
	}
}

// substituteGenericType replaces generic types with concrete types
func (cg *CodeGen) substituteGenericType(t parser.Type, typeMap map[string]parser.Type) parser.Type {
	switch typ := t.(type) {
	case parser.GenericType:
		if concrete, ok := typeMap[typ.Name]; ok {
			return concrete
		}
		return t
	case parser.ListType:
		return parser.ListType{Element: cg.substituteGenericType(typ.Element, typeMap)}
	case parser.FunctionType:
		newParams := make([]parser.Type, len(typ.Params))
		for i, param := range typ.Params {
			newParams[i] = cg.substituteGenericType(param, typeMap)
		}
		return parser.FunctionType{
			Params: newParams,
			Return: cg.substituteGenericType(typ.Return, typeMap),
		}
	default:
		return t
	}
}

// typeToString converts a type to a string for function name generation
func (cg *CodeGen) typeToString(t parser.Type) string {
	return typeToString(t)
}



// substituteGenericTypesInStmts recursively substitutes generic types in statements
func (cg *CodeGen) substituteGenericTypesInStmts(stmts []parser.Stmt, typeMap map[string]parser.Type) []parser.Stmt {
	result := make([]parser.Stmt, len(stmts))
	for i, stmt := range stmts {
		result[i] = cg.substituteGenericTypesInStmt(stmt, typeMap)
	}
	return result
}

// substituteGenericTypesInStmt recursively substitutes generic types in a statement
func (cg *CodeGen) substituteGenericTypesInStmt(stmt parser.Stmt, typeMap map[string]parser.Type) parser.Stmt {
	switch s := stmt.(type) {
	case *parser.ExprStmt:
		return &parser.ExprStmt{Expr: cg.substituteGenericTypesInExpr(s.Expr, typeMap)}

	case *parser.AssignStmt:
		return &parser.AssignStmt{
			Var:      s.Var,
			DeclType: cg.substituteGenericType(s.DeclType, typeMap),
			Expr:     cg.substituteGenericTypesInExpr(s.Expr, typeMap),
			Type:     cg.substituteGenericType(s.Type, typeMap),
		}
	default:
		return stmt // Return as-is for other statement types
	}
}

// substituteGenericTypesInExpr recursively substitutes generic types in an expression
func (cg *CodeGen) substituteGenericTypesInExpr(expr parser.Expr, typeMap map[string]parser.Type) parser.Expr {
	switch e := expr.(type) {
	case *parser.VarExpr:
		return &parser.VarExpr{
			Name: e.Name,
			Type: cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.NumberExpr:
		return &parser.NumberExpr{
			Value: e.Value,
			Type:  cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.BoolExpr:
		return &parser.BoolExpr{
			Value: e.Value,
			Type:  cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.StringExpr:
		return &parser.StringExpr{
			Value: e.Value,
			Type:  cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.BinaryExpr:
		return &parser.BinaryExpr{
			Op:    e.Op,
			Left:  cg.substituteGenericTypesInExpr(e.Left, typeMap),
			Right: cg.substituteGenericTypesInExpr(e.Right, typeMap),
			Type:  cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.UnaryExpr:
		return &parser.UnaryExpr{
			Op:   e.Op,
			Expr: cg.substituteGenericTypesInExpr(e.Expr, typeMap),
			Type: cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.PipeExpr:
		return &parser.PipeExpr{
			Left:  cg.substituteGenericTypesInExpr(e.Left, typeMap),
			Right: cg.substituteGenericTypesInExpr(e.Right, typeMap),
			Type:  cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.CallExpr:
		newArgs := make([]parser.Expr, len(e.Args))
		for i, arg := range e.Args {
			newArgs[i] = cg.substituteGenericTypesInExpr(arg, typeMap)
		}
		return &parser.CallExpr{
			Callee: cg.substituteGenericTypesInExpr(e.Callee, typeMap),
			Args:   newArgs,
			Type:   cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.ListExpr:
		newElements := make([]parser.Expr, len(e.Elements))
		for i, elem := range e.Elements {
			newElements[i] = cg.substituteGenericTypesInExpr(elem, typeMap)
		}
		return &parser.ListExpr{
			Elements: newElements,
			Type:     cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.RecordExpr:
		newFields := make(map[string]parser.Expr)
		for name, expr := range e.Fields {
			newFields[name] = cg.substituteGenericTypesInExpr(expr, typeMap)
		}
		return &parser.RecordExpr{
			Fields: newFields,
			Type:   cg.substituteGenericType(e.Type, typeMap),
		}
	case *parser.SpreadExpr:
		return &parser.SpreadExpr{
			Expr: cg.substituteGenericTypesInExpr(e.Expr, typeMap),
		}
	default:
		return expr // Return as-is for other expression types
	}
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
	case parser.GenericType:
		return ty.Name
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
	memcpy := m.NewFunc("llvm.memcpy.p0i8.p0i8.i64", types.Void, ir.NewParam("dst", types.NewPointer(types.I8)), ir.NewParam("src", types.NewPointer(types.I8)), ir.NewParam("len", types.I64), ir.NewParam("isvolatile", types.I1))
	strcmp := m.NewFunc("strcmp", types.I32, ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))
	memcmp := m.NewFunc("memcmp", types.I32, ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.I64))
	sprintf := m.NewFunc("sprintf", types.I32, ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))
	sprintf.Sig.Variadic = true
	exit := m.NewFunc("exit", types.Void, ir.NewParam("", types.I32))
	assertFailMsg := m.NewGlobalDef("assert_fail_msg", constant.NewCharArrayFromString("Assertion failed\n\x00"))

	return &CodeGen{module: m, vars: make(map[string]varInfo), functions: make(map[string]*ir.Func), genericFunctions: make(map[string]*parser.FunctionStmt), printf: printf, globalFmt: globalFmt, strFmt: strFmt, ifCounter: 0, whileCounter: 0, matchCounter: 0, stringCounter: 0, listPrintCounter: 0, recordPrintCounter: 0, intToStringCounter: 0, strlen: strlen, strcpy: strcpy, strcat: strcat, malloc: malloc, memcpy: memcpy, strcmp: strcmp, memcmp: memcmp, sprintf: sprintf, exit: exit, assertFailMsg: assertFailMsg}
}

func (cg *CodeGen) getParserType(expr parser.Expr) parser.Type {
	switch e := expr.(type) {
	case *parser.NumberExpr:
		return e.Type
	case *parser.BoolExpr:
		return e.Type
	case *parser.StringExpr:
		return e.Type
	case *parser.InterpolatedStringExpr:
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
		
		// Check if the list pointer is null (empty list)
		nullPtr := constant.NewNull(structPtr.Type().(*types.PointerType))
		isNull := bb.NewICmp(enum.IPredEQ, structPtr, nullPtr)
		cg.listPrintCounter++
		nullId := cg.listPrintCounter
		nullBB := bb.Parent.NewBlock(fmt.Sprintf("list_null_%d", nullId))
		notNullBB := bb.Parent.NewBlock(fmt.Sprintf("list_not_null_%d", nullId))
		bb.NewCondBr(isNull, nullBB, notNullBB)
		
		// Handle null list case
		nullBB = cg.printString(nullBB, "}")
		nullAfterBB := bb.Parent.NewBlock(fmt.Sprintf("list_null_after_%d", nullId))
		if addNewline {
			nullBB = cg.printString(nullBB, "\n")
		}
		nullBB.NewBr(nullAfterBB)
		
		// Handle non-null list case
		bb = notNullBB
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
		zeroBB = cg.printString(zeroBB, "")
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
		
		// Merge null and non-null paths
		finalAfterBB := bb.Parent.NewBlock(fmt.Sprintf("list_final_after_%d", nullId))
		afterBB.NewBr(finalAfterBB)
		nullAfterBB.NewBr(finalAfterBB)
		return finalAfterBB
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

	// First pass: declare all function signatures
	for _, stmt := range stmts {
		if s, ok := stmt.(*parser.FunctionStmt); ok {
			cg.declareFunctionSignature(s)
		}
	}

	// Second pass: generate function bodies and other statements
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *parser.FunctionStmt:
			cg.genFunctionBody(s)
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

func (cg *CodeGen) declareFunctionSignature(s *parser.FunctionStmt) {

	// Check if function has generic types - if so, store it for later instantiation
	for _, p := range s.Params {
		if cg.hasGenericType(p.Ty) {
	
			cg.genericFunctions[s.Name] = s
			return // Store generic functions for later instantiation
		}
	}
	if s.ReturnType != nil && cg.hasGenericType(s.ReturnType) {

		cg.genericFunctions[s.Name] = s
		return // Store generic functions for later instantiation
	}
	
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
}

func (cg *CodeGen) genFunctionBody(s *parser.FunctionStmt) {
	// Check if this is a generic function
	for _, p := range s.Params {
		if cg.hasGenericType(p.Ty) {
			return // Skip generic functions, they'll be instantiated later
		}
	}
	if s.ReturnType != nil && cg.hasGenericType(s.ReturnType) {
		return // Skip generic functions, they'll be instantiated later
	}
	
	// Get the already declared function
	f, ok := cg.functions[s.Name]
	if !ok {
		panic(fmt.Sprintf("function %s not declared", s.Name))
	}
	
	entry := f.NewBlock("entry")
	
	// Get the return type
	var rt types.Type = types.Void
	if s.ReturnType != nil {
		rt = cg.toLLVMType(s.ReturnType)
	}

	localVars := make(map[string]varInfo)
	for i, param := range f.Params {
		alloc := entry.NewAlloca(param.Type())
		entry.NewStore(param, alloc)
		localVars[s.Params[i].Name] = varInfo{Alloc: alloc, Typ: param.Type()}
	}

	current := cg.genStmts(entry, s.Body, localVars)

	if current.Term == nil {
		if rt.Equal(types.Void) {
			current.NewRet(nil)
		} else {
			// Handle implicit return from last expression
			if len(s.Body) > 0 {
				if lastExprStmt, ok := s.Body[len(s.Body)-1].(*parser.ExprStmt); ok {
					// Generate the last expression and return its value
					retVal, retBB := cg.genExpr(current, lastExprStmt.Expr, localVars)
					retBB.NewRet(retVal)
					return
				}
			}
			// Fallback to zero value if no expression to return
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
	case *parser.ImportStmt:
		// For now, imports are handled at parse time
		// In a full implementation, we would link imported modules
		return bb
	case *parser.TypeAliasStmt:
		// Type aliases are compile-time constructs, no runtime code needed
		return bb
	case *parser.AssignStmt:
		// Updated for typed variables
		val, bb := cg.genExpr(bb, s.Expr, vars)
		typ := cg.toLLVMType(s.Type)
		var alloc *ir.InstAlloca
		if _, ok := vars[s.Var]; ok {
			panic(fmt.Sprintf("variable %s already defined (variables are immutable)", s.Var))
		}
		alloc = bb.NewAlloca(typ)
		vars[s.Var] = varInfo{Alloc: alloc, Typ: typ}
		bb.NewStore(val, alloc)
		return bb

	case *parser.ExprStmt:
		_, bb = cg.genExpr(bb, s.Expr, vars) // Generate but ignore result
		return bb
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

	case *parser.AssertStmt:
		cg.assertCounter++
		id := cg.assertCounter
		condVal, bb := cg.genExpr(bb, s.Condition, vars)
		condTyp := condVal.Type()
		var cond value.Value
		if condTyp.Equal(types.I1) {
			cond = condVal
		} else if condTyp.Equal(types.I64) {
			zero := constant.NewInt(types.I64, 0)
			cond = bb.NewICmp(enum.IPredNE, condVal, zero)
		} else {
			panic("invalid condition type in assert")
		}

		failBB := bb.Parent.NewBlock(fmt.Sprintf("assert_fail_%d", id))
		passBB := bb.Parent.NewBlock(fmt.Sprintf("assert_pass_%d", id))
		
		bb.NewCondBr(cond, passBB, failBB)
		
		// Generate assertion failure block
		failBB.NewCall(cg.printf, cg.assertFailMsg)
		failBB.NewCall(cg.exit, constant.NewInt(types.I32, 1))
		failBB.NewUnreachable()
		
		return passBB

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
	case *parser.InterpolatedStringExpr:
		return cg.generateInterpolatedString(e, bb, vars)
	case *parser.VarExpr:
		v, ok := vars[e.Name]
		if ok {
			return bb.NewLoad(cg.toLLVMType(e.Type), v.Alloc), bb
		}
		f, ok := cg.functions[e.Name]
		if ok {
			return f, bb
		}
		
		// Check if this is a generic function that needs instantiation
		if _, exists := cg.genericFunctions[e.Name]; exists {
			// For now, try to find the already specialized version
			// This is a hack - ideally the type checker should resolve generics
			for funcName := range cg.functions {
				if len(funcName) > len(e.Name) && funcName[:len(e.Name)] == e.Name && funcName[len(e.Name)] == '_' {
					// Found a specialized version, use it
					return cg.functions[funcName], bb
				}
			}
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
		} else if e.Type == parser.BasicType("string") {
			if e.Op == parser.TokenPlus {
				leftLen := bb.NewCall(cg.strlen, left)
				rightLen := bb.NewCall(cg.strlen, right)
				totalLen := bb.NewAdd(leftLen, rightLen)
				alloc := bb.NewCall(cg.malloc, bb.NewAdd(totalLen, constant.NewInt(types.I64, 1)))
				bb.NewCall(cg.strcpy, alloc, left)
				bb.NewCall(cg.strcat, alloc, right)
				return alloc, bb
			} else if e.Op == parser.TokenEQ {
				strcmp := cg.module.NewFunc("strcmp", types.I32, ir.NewParam("", types.NewPointer(types.I8)), ir.NewParam("", types.NewPointer(types.I8)))
				result := bb.NewCall(strcmp, left, right)
				return bb.NewICmp(enum.IPredEQ, result, constant.NewInt(types.I32, 0)), bb
			}
		} else if lt, ok := e.Type.(parser.ListType); ok && e.Op == parser.TokenEQ {
			return cg.genListEquality(bb, left, right, lt.Element)
		} else {
			panic("unsupported binary operation")
			return nil, bb
		}
	case *parser.PipeExpr:
		// Transform pipe expr: left |> right into right(left)
		// If right is a function call, prepend left to its args
		if callExpr, ok := e.Right.(*parser.CallExpr); ok {
			// Create new args with left expression first
			newArgs := []parser.Expr{e.Left}
			newArgs = append(newArgs, callExpr.Args...)
			newCallExpr := &parser.CallExpr{
				Callee: callExpr.Callee,
				Args:   newArgs,
				Type:   e.Type,
			}
			return cg.genExpr(bb, newCallExpr, vars)
		} else {
			// If right is just a function name, call it with left as argument
			newCallExpr := &parser.CallExpr{
				Callee: e.Right,
				Args:   []parser.Expr{e.Left},
				Type:   e.Type,
			}
			return cg.genExpr(bb, newCallExpr, vars)
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
			} else if calleeName == "len" {
				listPtr, bb := cg.genExpr(bb, e.Args[0], vars)
				structTy := listPtr.Type().(*types.PointerType).ElemType
				lengthPtr := bb.NewGetElementPtr(structTy, listPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
				length := bb.NewLoad(types.I64, lengthPtr)
				return length, bb
			} else if calleeName == "compare" {
				if len(e.Args) != 2 {
					panic("compare takes two arguments")
				}
				left, bb := cg.genExpr(bb, e.Args[0], vars)
				right, bb := cg.genExpr(bb, e.Args[1], vars)
				pty := cg.getParserType(e.Args[0])
				
				// Handle list and record comparison specially to avoid terminator issues
				if listType, ok := pty.(parser.ListType); ok {
					return cg.genListEquality(bb, left, right, listType.Element)
				} else if recordType, ok := pty.(parser.RecordType); ok {
					return cg.genRecordEquality(bb, left, right, recordType)
				} else {
					result := cg.genCompare(bb, left, right, pty)
					return result, bb
				}
			} else if vi, ok := vars[calleeName]; ok {
				callee = bb.NewLoad(vi.Typ, vi.Alloc)
			} else if f, ok := cg.functions[calleeName]; ok {
				callee = f
			} else if _, ok := cg.genericFunctions[calleeName]; ok {
		
				// Pre-specialize any generic function arguments
				for i, arg := range e.Args {
					if varExpr, ok := arg.(*parser.VarExpr); ok {
						if _, isGeneric := cg.genericFunctions[varExpr.Name]; isGeneric {
							// This is a generic function used as an argument
							// Try to infer the concrete type from the function signature
							if _, isFuncType := varExpr.Type.(parser.FunctionType); isFuncType {
								// If the function type has any concrete types, use them for specialization
								// For identity function of type fn(t) -> t, we need to infer t from context
								// Look at other arguments to infer the type
								for j, otherArg := range e.Args {
									if j != i {
										otherType := cg.getParserType(otherArg)
										if basic, isBasic := otherType.(parser.BasicType); isBasic {
											// Found a concrete type, try to specialize the generic function
											var specializationTypes []parser.Type
											specializationTypes = append(specializationTypes, basic)
											cg.instantiateGenericFunction(varExpr.Name, specializationTypes)
										}
									}
								}
							}
						}
					}
				}
		
				// Try to instantiate generic function
				var argTypes []parser.Type
				for _, arg := range e.Args {
					argTypes = append(argTypes, cg.getParserType(arg))
				}
				f := cg.instantiateGenericFunction(calleeName, argTypes)
				if f != nil {
					callee = f
				} else {
					panic("failed to instantiate generic function: " + calleeName)
				}
			} else {
				panic("undefined: " + calleeName)
			}
		} else {
			callee, bb = cg.genExpr(bb, e.Callee, vars)
		}
		
		// Only process arguments for regular function calls, not built-ins
		if callee != nil {
			var args []value.Value
			for _, arg := range e.Args {
				var argVal value.Value
				argVal, bb = cg.genExpr(bb, arg, vars)
				args = append(args, argVal)
			}
			return bb.NewCall(callee, args...), bb
		}
		
		// This should not be reached as all cases above should return
		panic("unreachable code in CallExpr generation")
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
		cg.listPrintCounter++
		id := cg.listPrintCounter
		totalLengthAlloc := bb.NewAlloca(types.I64)
		bb.NewStore(constant.NewInt(types.I64, 0), totalLengthAlloc)
		currentBB := bb
		for _, el := range e.Elements {
			if se, ok := el.(*parser.SpreadExpr); ok {
				// Skip empty list spreads
				if le, ok := se.Expr.(*parser.ListExpr); ok && len(le.Elements) == 0 {
					continue
				}
				spreadVal, newBB := cg.genExpr(currentBB, se.Expr, vars)
				currentBB = newBB
				spreadStructTy := spreadVal.Type().(*types.PointerType).ElemType
				
				// Check if the list pointer is null (empty list)
				nullPtr := constant.NewNull(spreadVal.Type().(*types.PointerType))
				isNull := currentBB.NewICmp(enum.IPredEQ, spreadVal, nullPtr)
				notNullBB := currentBB.Parent.NewBlock(fmt.Sprintf("spread_not_null_%d", id))
				contBB := currentBB.Parent.NewBlock(fmt.Sprintf("spread_cont_%d", id))
				currentBB.NewCondBr(isNull, contBB, notNullBB)
				
				// Not null block - read length
				currentBB = notNullBB
				spreadLengthPtr := currentBB.NewGetElementPtr(spreadStructTy, spreadVal, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
				spreadLength := currentBB.NewLoad(types.I64, spreadLengthPtr)
				currentTotal := currentBB.NewLoad(types.I64, totalLengthAlloc)
				newTotal := currentBB.NewAdd(currentTotal, spreadLength)
				currentBB.NewStore(newTotal, totalLengthAlloc)
				currentBB.NewBr(contBB)
				
				// Continue block
				currentBB = contBB
			} else {
				currentTotal := currentBB.NewLoad(types.I64, totalLengthAlloc)
				newTotal := currentBB.NewAdd(currentTotal, constant.NewInt(types.I64, 1))
				currentBB.NewStore(newTotal, totalLengthAlloc)
			}
		}
		totalLength := currentBB.NewLoad(types.I64, totalLengthAlloc)
		isZeroAfter := currentBB.NewICmp(enum.IPredEQ, totalLength, constant.NewInt(types.I64, 0))
		zeroBB := currentBB.Parent.NewBlock(fmt.Sprintf("list_zero_%d", id))
		allocBB := currentBB.Parent.NewBlock(fmt.Sprintf("list_alloc_%d", id))
		currentBB.NewCondBr(isZeroAfter, zeroBB, allocBB)
		bb = allocBB
		elemSizeTy := types.NewArray(1, elemTy)
		nullPtrElem := constant.NewNull(types.NewPointer(elemSizeTy))
		sizePtrElem := bb.NewGetElementPtr(elemSizeTy, nullPtrElem, constant.NewInt(types.I64, 1))
		sizePtrElem.InBounds = true
		elemSize := bb.NewPtrToInt(sizePtrElem, types.I64)
		totalLengthInAlloc := bb.NewLoad(types.I64, totalLengthAlloc)
		arraySize := bb.NewMul(totalLengthInAlloc, elemSize)
		mallocedArray := bb.NewCall(cg.malloc, arraySize)
		arrayAlloc := bb.NewBitCast(mallocedArray, types.NewPointer(elemTy))
		currentPtrAlloc := bb.NewAlloca(types.NewPointer(elemTy))
		bb.NewStore(arrayAlloc, currentPtrAlloc)
		for _, el := range e.Elements {
			if se, ok := el.(*parser.SpreadExpr); ok {
				// Skip empty list spreads
				if le, ok := se.Expr.(*parser.ListExpr); ok && len(le.Elements) == 0 {
					continue
				}
				spreadVal, newBB := cg.genExpr(bb, se.Expr, vars)
				bb = newBB
				spreadStructTy := spreadVal.Type().(*types.PointerType).ElemType
				
				// Check if the list pointer is null (empty list)  
				nullPtr := constant.NewNull(spreadVal.Type().(*types.PointerType))
				isNull := bb.NewICmp(enum.IPredEQ, spreadVal, nullPtr)
				notNullBB := bb.Parent.NewBlock(fmt.Sprintf("spread_not_null_copy_%d", id))
				skipBB := bb.Parent.NewBlock(fmt.Sprintf("spread_skip_copy_%d", id))
				bb.NewCondBr(isNull, skipBB, notNullBB)
				
				// Not null block - check length and copy if needed
				bb = notNullBB
				spreadLengthPtr := bb.NewGetElementPtr(spreadStructTy, spreadVal, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
				spreadLength := bb.NewLoad(types.I64, spreadLengthPtr)
				
				// Only copy if the list is not empty
				isNonEmpty := bb.NewICmp(enum.IPredNE, spreadLength, constant.NewInt(types.I64, 0))
				copyBB := bb.Parent.NewBlock(fmt.Sprintf("spread_copy_%d", id))
				nextBB := bb.Parent.NewBlock(fmt.Sprintf("spread_next_%d", id))
				bb.NewCondBr(isNonEmpty, copyBB, nextBB)
				
				// Copy block
				bb = copyBB
				spreadDataPtrPtr := bb.NewGetElementPtr(spreadStructTy, spreadVal, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
				spreadDataPtr := bb.NewLoad(types.NewPointer(elemTy), spreadDataPtrPtr)
				spreadSize := bb.NewMul(spreadLength, elemSize)
				currentPtrLoaded := bb.NewLoad(types.NewPointer(elemTy), currentPtrAlloc)
				dst := bb.NewBitCast(currentPtrLoaded, types.NewPointer(types.I8))
				src := bb.NewBitCast(spreadDataPtr, types.NewPointer(types.I8))
				bb.NewCall(cg.memcpy, dst, src, spreadSize, constant.NewBool(false))
				newCurrent := bb.NewGetElementPtr(elemTy, currentPtrLoaded, spreadLength)
				bb.NewStore(newCurrent, currentPtrAlloc)
				bb.NewBr(nextBB)
				
				// Next block
				bb = nextBB
				bb.NewBr(skipBB)
				
				// Skip block - continue to next element
				bb = skipBB
			} else {
				val, newBB := cg.genExpr(bb, el, vars)
				bb = newBB
				currentPtrLoaded := bb.NewLoad(types.NewPointer(elemTy), currentPtrAlloc)
				bb.NewStore(val, currentPtrLoaded)
				newCurrent := bb.NewGetElementPtr(elemTy, currentPtrLoaded, constant.NewInt(types.I64, 1))
				bb.NewStore(newCurrent, currentPtrAlloc)
			}
		}
		structTy := types.NewStruct(types.I64, types.NewPointer(elemTy))
		nullPtrStruct := constant.NewNull(types.NewPointer(structTy))
		sizePtrStruct := bb.NewGetElementPtr(structTy, nullPtrStruct, constant.NewInt(types.I64, 1))
		sizePtrStruct.InBounds = true
		sizeStruct := bb.NewPtrToInt(sizePtrStruct, types.I64)
		mallocedStruct := bb.NewCall(cg.malloc, sizeStruct)
		structAlloc := bb.NewBitCast(mallocedStruct, types.NewPointer(structTy))
		lengthPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		totalLengthFinal := bb.NewLoad(types.I64, totalLengthAlloc)
		bb.NewStore(totalLengthFinal, lengthPtr)
		dataFieldPtr := bb.NewGetElementPtr(structTy, structAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
		bb.NewStore(arrayAlloc, dataFieldPtr)
		allocEndBB := bb
		afterBB := bb.Parent.NewBlock(fmt.Sprintf("list_after_%d", id))
		allocEndBB.NewBr(afterBB)
		zeroBB.NewBr(afterBB)
		
		// Switch to the afterBB and create the phi node
		bb = afterBB
		nullList := constant.NewNull(types.NewPointer(structTy))
		result := bb.NewPhi(ir.NewIncoming(nullList, zeroBB), ir.NewIncoming(structAlloc, allocEndBB))
		return result, bb
	case *parser.SpreadExpr:
		val, bb := cg.genExpr(bb, e.Expr, vars)
		return val, bb
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
	default:
		panic("unknown expression type")
		return nil, bb
	}
	return nil, bb
}

func (cg *CodeGen) genListEquality(bb *ir.Block, left, right value.Value, elemType parser.Type) (value.Value, *ir.Block) {
	structType := left.Type().(*types.PointerType).ElemType
	leftLengthPtr := bb.NewGetElementPtr(structType, left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	leftLength := bb.NewLoad(types.I64, leftLengthPtr)
	rightLengthPtr := bb.NewGetElementPtr(structType, right, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	rightLength := bb.NewLoad(types.I64, rightLengthPtr)
	lengthEq := bb.NewICmp(enum.IPredEQ, leftLength, rightLength)
	cg.listPrintCounter++
	id := cg.listPrintCounter
	notEqBB := bb.Parent.NewBlock(fmt.Sprintf("list_not_eq_%d", id))
	checkElementsBB := bb.Parent.NewBlock(fmt.Sprintf("list_check_elements_%d", id))
	finalEqBB := bb.Parent.NewBlock(fmt.Sprintf("list_eq_final_%d", id))
	bb.NewCondBr(lengthEq, checkElementsBB, notEqBB)
	notEqBB.NewBr(finalEqBB)
	bb = checkElementsBB
	zero := constant.NewInt(types.I64, 0)
	isZero := bb.NewICmp(enum.IPredEQ, leftLength, zero)
	zeroBB := bb.Parent.NewBlock(fmt.Sprintf("list_zero_eq_%d", id))
	loopCondBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_cond_eq_%d", id))
	loopBodyBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_body_eq_%d", id))
	loopIncBB := bb.Parent.NewBlock(fmt.Sprintf("list_loop_inc_eq_%d", id))
	eqBB := bb.Parent.NewBlock(fmt.Sprintf("list_eq_%d", id))
	bb.NewCondBr(isZero, zeroBB, loopCondBB)
	zeroBB.NewBr(finalEqBB)

	i := loopCondBB.NewPhi(ir.NewIncoming(zero, checkElementsBB))
	cmp := loopCondBB.NewICmp(enum.IPredSLT, i, leftLength)
	loopCondBB.NewCondBr(cmp, loopBodyBB, eqBB)
	eqBB.NewBr(finalEqBB)
	leftDataPtrPtr := loopBodyBB.NewGetElementPtr(structType, left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	leftDataPtr := loopBodyBB.NewLoad(leftDataPtrPtr.Type().(*types.PointerType).ElemType, leftDataPtrPtr)
	rightDataPtrPtr := loopBodyBB.NewGetElementPtr(structType, right, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	rightDataPtr := loopBodyBB.NewLoad(rightDataPtrPtr.Type().(*types.PointerType).ElemType, rightDataPtrPtr)
	elemTy := cg.toLLVMType(elemType)
	leftElemPtr := loopBodyBB.NewGetElementPtr(elemTy, leftDataPtr, i)
	leftElemPtr.InBounds = true
	leftElem := loopBodyBB.NewLoad(elemTy, leftElemPtr)
	rightElemPtr := loopBodyBB.NewGetElementPtr(elemTy, rightDataPtr, i)
	rightElemPtr.InBounds = true
	rightElem := loopBodyBB.NewLoad(elemTy, rightElemPtr)
	var elemEq value.Value
	if _, ok := elemType.(parser.BasicType); ok {
		elemEq = loopBodyBB.NewICmp(enum.IPredEQ, leftElem, rightElem)
	} else if nestedLt, ok := elemType.(parser.ListType); ok {
		elemEq, loopBodyBB = cg.genListEquality(loopBodyBB, leftElem, rightElem, nestedLt.Element)
	} else if nestedRt, ok := elemType.(parser.RecordType); ok {
		elemEq, loopBodyBB = cg.genRecordEquality(loopBodyBB, leftElem, rightElem, nestedRt)
	} else {
		panic("unsupported type for list equality")
	}
	notEqElemBB := loopBodyBB.Parent.NewBlock(fmt.Sprintf("list_not_eq_elem_%d", id))
	loopBodyBB.NewCondBr(elemEq, loopIncBB, notEqElemBB)
	notEqElemBB.NewBr(finalEqBB)
	incI := loopIncBB.NewAdd(i, constant.NewInt(types.I64, 1))
	loopIncBB.NewBr(loopCondBB)
	i.Incs = append(i.Incs, ir.NewIncoming(incI, loopIncBB))
	
	// Create phi node for final result
	result := finalEqBB.NewPhi(
		ir.NewIncoming(constant.NewBool(false), notEqBB),
		ir.NewIncoming(constant.NewBool(true), zeroBB),
		ir.NewIncoming(constant.NewBool(true), eqBB),
		ir.NewIncoming(constant.NewBool(false), notEqElemBB),
	)
	return result, finalEqBB
}

func (cg *CodeGen) genRecordEquality(bb *ir.Block, left, right value.Value, recType parser.RecordType) (value.Value, *ir.Block) {
	structType := left.Type().(*types.PointerType).ElemType
	cg.recordPrintCounter++
	id := cg.recordPrintCounter
	notEqRecFinalBB := bb.Parent.NewBlock(fmt.Sprintf("rec_not_eq_final_%d", id))
	var lastEqBB *ir.Block
	for i, f := range recType.Fields {
		leftFieldPtr := bb.NewGetElementPtr(structType, left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(i)))
		leftFieldPtr.InBounds = true
		leftField := bb.NewLoad(leftFieldPtr.Type().(*types.PointerType).ElemType, leftFieldPtr)
		rightFieldPtr := bb.NewGetElementPtr(structType, right, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, int64(i)))
		rightFieldPtr.InBounds = true
		rightField := bb.NewLoad(rightFieldPtr.Type().(*types.PointerType).ElemType, rightFieldPtr)
		var fieldEq value.Value
		if f.Optional {
			leftNull := bb.NewICmp(enum.IPredEQ, leftField, constant.NewNull(leftField.Type().(*types.PointerType)))
			rightNull := bb.NewICmp(enum.IPredEQ, rightField, constant.NewNull(rightField.Type().(*types.PointerType)))
			bothNull := bb.NewAnd(leftNull, rightNull)
			notEqBB := bb.Parent.NewBlock(fmt.Sprintf("rec_not_eq_null_%d_%d", id, i))
			checkValBB := bb.Parent.NewBlock(fmt.Sprintf("rec_check_val_%d_%d", id, i))
			bb.NewCondBr(bothNull, checkValBB, notEqBB)
			notEqBB.NewBr(notEqRecFinalBB)
			bb = checkValBB
			leftNotNull := bb.NewICmp(enum.IPredNE, leftField, constant.NewNull(leftField.Type().(*types.PointerType)))
			rightNotNull := bb.NewICmp(enum.IPredNE, rightField, constant.NewNull(rightField.Type().(*types.PointerType)))
			bothNotNull := bb.NewAnd(leftNotNull, rightNotNull)
			notEqNullBB := bb.Parent.NewBlock(fmt.Sprintf("rec_not_eq_null_val_%d_%d", id, i))
			loadValBB := bb.Parent.NewBlock(fmt.Sprintf("rec_load_val_%d_%d", id, i))
			bb.NewCondBr(bothNotNull, loadValBB, notEqNullBB)
			notEqNullBB.NewBr(notEqRecFinalBB)
			bb = loadValBB
			leftField = bb.NewLoad(leftField.Type().(*types.PointerType).ElemType, leftField)
			rightField = bb.NewLoad(rightField.Type().(*types.PointerType).ElemType, rightField)
		}
		if _, ok := f.Ty.(parser.BasicType); ok {
			fieldEq = bb.NewICmp(enum.IPredEQ, leftField, rightField)
		} else if nestedLt, ok := f.Ty.(parser.ListType); ok {
			fieldEq, bb = cg.genListEquality(bb, leftField, rightField, nestedLt.Element)
		} else if nestedRt, ok := f.Ty.(parser.RecordType); ok {
			fieldEq, bb = cg.genRecordEquality(bb, leftField, rightField, nestedRt)
		} else {
			panic("unsupported type for record equality")
		}
		notEqFieldBB := bb.Parent.NewBlock(fmt.Sprintf("rec_not_eq_field_%d_%d", id, i))
		if i < len(recType.Fields)-1 {
			nextFieldBB := bb.Parent.NewBlock(fmt.Sprintf("rec_next_field_%d_%d", id, i+1))
			bb.NewCondBr(fieldEq, nextFieldBB, notEqFieldBB)
			bb = nextFieldBB
		} else {
			lastEqBB = bb.Parent.NewBlock(fmt.Sprintf("rec_eq_%d", id))
			bb.NewCondBr(fieldEq, lastEqBB, notEqFieldBB)
		}
		notEqFieldBB.NewBr(notEqRecFinalBB)
	}
	
	// Create final blocks and phi node for result
	finalRecEqBB := bb.Parent.NewBlock(fmt.Sprintf("rec_eq_final_%d", id))
	notEqRecFinalBB.NewBr(finalRecEqBB)
	
	// Add terminator to the eqBB that was created in the last iteration
	if lastEqBB != nil {
		lastEqBB.NewBr(finalRecEqBB)
	}
	
	result := finalRecEqBB.NewPhi(
		ir.NewIncoming(constant.NewBool(false), notEqRecFinalBB),
		ir.NewIncoming(constant.NewBool(true), lastEqBB),
	)
	return result, finalRecEqBB
}

func (cg *CodeGen) genCompare(bb *ir.Block, left, right value.Value, pty parser.Type) value.Value {
	switch ty := pty.(type) {
	case parser.BasicType:
		switch string(ty) {
		case "int", "bool":
			return bb.NewICmp(enum.IPredEQ, left, right)
		case "string":
			result := bb.NewCall(cg.strcmp, left, right)
			return bb.NewICmp(enum.IPredEQ, result, constant.NewInt(types.I32, 0))
		}
	case parser.ListType:
		// This should not be called for compare function, handled separately
		panic("list comparison should be handled in compare function call")
	case parser.RecordType:
		// This should not be called for compare function, handled separately
		panic("record comparison should be handled in compare function call")
	}
	panic("unsupported type for comparison")
	return constant.NewBool(false)
}

func (cg *CodeGen) generateInterpolatedString(e *parser.InterpolatedStringExpr, bb *ir.Block, vars map[string]varInfo) (value.Value, *ir.Block) {
	if len(e.Parts) == 0 {
		// Empty interpolated string
		strConst := constant.NewCharArrayFromString("\x00")
		cg.stringCounter++
		name := fmt.Sprintf("str_%d", cg.stringCounter)
		globalStr := cg.module.NewGlobalDef(name, strConst)
		elemType := globalStr.Type().(*types.PointerType).ElemType
		ptr := bb.NewGetElementPtr(elemType, globalStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		ptr.InBounds = true
		return ptr, bb
	}

	// First, calculate total length needed
	var totalLen value.Value = constant.NewInt(types.I64, 1) // for null terminator

	// Arrays to store string parts and their lengths
	var stringParts []value.Value
	var partLengths []value.Value

	for _, part := range e.Parts {
		if part.IsExpr {
			// Generate code for the expression
			exprVal, newBB := cg.genExpr(bb, part.Expr, vars)
			bb = newBB
			
			// Convert expression to string
			var strVal value.Value
			switch cg.getParserType(part.Expr).(parser.BasicType) {
			case "int":
				// Use sprintf to convert int to string
				bufSize := constant.NewInt(types.I64, 32) // enough for most integers
				buf := bb.NewCall(cg.malloc, bufSize)
				fmtStr := cg.getIntFormatString()
				elemType := fmtStr.Type().(*types.PointerType).ElemType
				fmtPtr := bb.NewGetElementPtr(elemType, fmtStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
				fmtPtr.InBounds = true
				bb.NewCall(cg.sprintf, buf, fmtPtr, exprVal)
				strVal = buf
			case "string":
				strVal = exprVal
			default:
				// For other types, convert to string representation
				strVal = exprVal
			}
			
			// Get length of the string part
			partLen := bb.NewCall(cg.strlen, strVal)
			
			stringParts = append(stringParts, strVal)
			partLengths = append(partLengths, partLen)
			totalLen = bb.NewAdd(totalLen, partLen)
		} else {
			// Static text part
			if part.Text != "" {
				strConst := constant.NewCharArrayFromString(part.Text + "\x00")
				cg.stringCounter++
				name := fmt.Sprintf("str_%d", cg.stringCounter)
				globalStr := cg.module.NewGlobalDef(name, strConst)
				elemType := globalStr.Type().(*types.PointerType).ElemType
				ptr := bb.NewGetElementPtr(elemType, globalStr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
				ptr.InBounds = true
				
				partLen := constant.NewInt(types.I64, int64(len(part.Text)))
				
				stringParts = append(stringParts, ptr)
				partLengths = append(partLengths, partLen)
				totalLen = bb.NewAdd(totalLen, partLen)
			}
		}
	}

	// Allocate memory for the result string
	result := bb.NewCall(cg.malloc, totalLen)
	
	// Initialize result string as empty
	emptyStr := constant.NewCharArrayFromString("\x00")
	cg.stringCounter++
	emptyName := fmt.Sprintf("str_%d", cg.stringCounter)
	globalEmpty := cg.module.NewGlobalDef(emptyName, emptyStr)
	elemType := globalEmpty.Type().(*types.PointerType).ElemType
	emptyPtr := bb.NewGetElementPtr(elemType, globalEmpty, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	emptyPtr.InBounds = true
	bb.NewCall(cg.strcpy, result, emptyPtr)

	// Concatenate all parts
	for _, part := range stringParts {
		bb.NewCall(cg.strcat, result, part)
	}

	return result, bb
}

func (cg *CodeGen) getIntFormatString() *ir.Global {
	if cg.intFmt == nil {
		fmtStr := constant.NewCharArrayFromString("%lld\x00")
		cg.intFmt = cg.module.NewGlobalDef("int_fmt", fmtStr)
	}
	return cg.intFmt
}
