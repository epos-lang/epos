package parser

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

// Token types
type TokenType int

const (
	TokenEOF TokenType = iota
	TokenNumber
	TokenIdentifier
	TokenPlus
	TokenMinus
	TokenDiv
	TokenMod
	TokenAssign

	TokenSemicolon
	TokenLParen
	TokenRParen
	TokenFunction
	TokenEnd
	TokenComma
	TokenGT
	TokenLT
	TokenGTE
	TokenLTE
	TokenEQ
	TokenString
	TokenMatch
	TokenThen
	TokenDefault
	TokenArrow
	TokenTrue
	TokenFalse
	TokenLBracket
	TokenRBracket
	TokenColon
	TokenTypeInt
	TokenTypeString
	TokenTypeList
	TokenTypeBool
	TokenTypeFloat
	TokenLBrace
	TokenRBrace
	TokenRecord
	TokenAt
	TokenFatArrow
	TokenQuestion
	TokenDot
	TokenSpread
	TokenInterpolatedString
	TokenAssert
	TokenImport
	TokenFrom
	TokenAsterisk
	TokenMul
	TokenTypeKeyword
	TokenPipe
	TokenUnion
	TokenNot
	TokenNeq
	TokenAnd
	TokenOr
	TokenTypeFile
	TokenWalrus
)

// Token struct
type Token struct {
	Type  TokenType
	Value string
}

// AST nodes
type Expr interface{}

type NumberExpr struct {
	Value int64
	Type  Type
}

type FloatExpr struct {
	Value float64
	Type  Type
}

type VarExpr struct {
	Name string
	Type Type
}

type BinaryExpr struct {
	Op    TokenType
	Left  Expr
	Right Expr
	Type  Type
}

type UnaryExpr struct {
	Op   TokenType
	Expr Expr
	Type Type
}

type Stmt interface{}

type LambdaExpr struct {
	Params []Param
	ReturnType Type
	Body Expr
	Type Type
}

type AssignStmt struct {
	Var      string
	DeclType Type
	Expr     Expr
	Type     Type
}

type FunctionStmt struct {
	Name       string
	Params     []Param
	ReturnType Type
	Body       []Stmt
	IsPublic   bool
}

type CallExpr struct {
	Callee Expr
	Args   []Expr
	Type   Type
}

type PipeExpr struct {
	Left  Expr
	Right Expr
	Type  Type
}



type MatchCase struct {
	Values []Expr
	Body   Stmt
}

type MatchStmt struct {
	Expr    Expr
	Cases   []MatchCase
	Default Stmt
}

type MatchCaseExpr struct {
	Values []Expr
	Body   Expr
}

type MatchExpr struct {
	Expr    Expr
	Cases   []MatchCaseExpr
	Default Expr
	Type    Type
}

type BoolExpr struct {
	Value bool
	Type  Type
}

type StringExpr struct {
	Value string
	Type  Type
}

type InterpolatedStringExpr struct {
	Parts []StringPart
	Type  Type
}

type StringPart struct {
	IsExpr bool
	Text   string
	Expr   Expr
}

type ListExpr struct {
	Elements []Expr
	Type     Type
}

type SpreadExpr struct {
	Expr Expr
	Type Type
}

type RangeExpr struct {
	Start Expr
	End   Expr
	Type  Type
}

type Type interface{}

type BasicType string

type ListType struct {
	Element Type
}

type Field struct {
	Name     string
	Ty       Type
	Optional bool
}

type RecordType struct {
	Fields     []Field
	TypeParams []string // Generic type parameters like "t" in record value(t)
}

type FunctionType struct {
	Params []Type
	Return Type
}

type GenericType struct {
	Name string
}

func (g GenericType) String() string {
	return g.Name
}

type GenericInstType struct {
	Name     string   // The generic type name (e.g., "value")
	TypeArgs []Type   // The concrete types (e.g., [int])
}

func (g GenericInstType) String() string {
	args := make([]string, len(g.TypeArgs))
	for i, arg := range g.TypeArgs {
		args[i] = fmt.Sprintf("%v", arg)
	}
	return g.Name + "(" + strings.Join(args, ", ") + ")"
}

type UnionType struct {
	Types []Type
}

func (u UnionType) String() string {
	typeStrs := make([]string, len(u.Types))
	for i, t := range u.Types {
		typeStrs[i] = fmt.Sprintf("%v", t)
	}
	return strings.Join(typeStrs, " | ")
}

type RecordExpr struct {
	Fields map[string]Expr
	Type   Type
}

type FieldAccessExpr struct {
	Receiver Expr
	Field    string
	Type     Type
}

type TypeAliasStmt struct {
	Name string
	Ty   Type
}

type RecordDecl struct {
	Name   string
	Fields []Field
}

type Param struct {
	Name    string
	Ty      Type
	Default Expr  // nil if no default value
}

type ExprStmt struct {
	Expr Expr
	Type Type
}

type AssertStmt struct {
	Condition Expr
	Type      Type
}

type ImportStmt struct {
	Path       string
	Items      []string  // specific imports like {function1, function2}
	Alias      string    // for single imports like "import function from ..."
	IsWildcard bool      // for simple imports like "import ./file.epos"
}

// Lexer
type Lexer struct {
	input  string
	pos    int
	tokens []Token
}

func NewLexer(input string) *Lexer {
	return &Lexer{input: input, pos: 0}
}

func (l *Lexer) Lex() []Token {
	l.tokens = []Token{}
	for l.pos < len(l.input) {
		ch := l.input[l.pos]
		switch {
		case unicode.IsSpace(rune(ch)):
			l.pos++
		case unicode.IsDigit(rune(ch)):
			l.lexNumber()
		case unicode.IsLetter(rune(ch)):
			l.lexIdentifier()
		case ch == '"':
			l.lexStringOrInterpolated()
		case ch == '[':
			if l.peekChar() == '[' {
				l.lexMultiLineStringOrInterpolated()
			} else {
				l.addToken(TokenLBracket, "[")
			}
		case ch == '+':
			l.addToken(TokenPlus, "+")
		case ch == '#':
			l.lexComment()
			continue
		case ch == '-':
			if l.peekChar() == '>' {
				l.pos++
				l.addToken(TokenArrow, "->")
			} else {
				l.addToken(TokenMinus, "-")
			}
		case ch == '*':
			// Check if this is a visibility marker in function declaration
			if len(l.tokens) >= 2 && 
				l.tokens[len(l.tokens)-1].Type == TokenIdentifier &&
				l.tokens[len(l.tokens)-2].Type == TokenFunction {
				l.addToken(TokenAsterisk, "*")
			} else {
				l.addToken(TokenMul, "*")
			}
		case ch == '/':
			l.addToken(TokenDiv, "/")
		case ch == '%':
			l.addToken(TokenMod, "%")
		case ch == '=':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenEQ, "==")
			} else if l.peekChar() == '>' {
				l.pos++
				l.addToken(TokenFatArrow, "=>")
			} else {
				l.addToken(TokenAssign, "=")
			}
		case ch == '>':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenGTE, ">=")
			} else {
				l.addToken(TokenGT, ">")
			}
		case ch == '<':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenLTE, "<=")
			} else {
				l.addToken(TokenLT, "<")
			}
		case ch == ',':
			l.addToken(TokenComma, ",")
		case ch == ';':
			l.addToken(TokenSemicolon, ";")
		case ch == '_':
			l.addToken(TokenDefault, "_")
		case ch == '(':
			if l.peekChar() == '#' {
				l.pos++ // skip (
				l.lexMultiLineComment()
				continue
			} else {
				l.addToken(TokenLParen, "(")
			}
		case ch == ')':
			l.addToken(TokenRParen, ")")
		case ch == ']':
			l.addToken(TokenRBracket, "]")
		case ch == '{':
			l.addToken(TokenLBrace, "{")
		case ch == '}':
			l.addToken(TokenRBrace, "}")
		case ch == ':':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenWalrus, ":=")
			} else {
				l.addToken(TokenColon, ":")
			}
		case ch == '?':
			l.addToken(TokenQuestion, "?")
		case ch == '@':
			l.addToken(TokenAt, "@")
		case ch == '.':
			if l.peekChar() == '.' {
				l.addToken(TokenSpread, "..")
			} else {
				l.addToken(TokenDot, ".")
			}
		case ch == '!':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenNeq, "!=")
			} else {
				panic(fmt.Sprintf("unexpected character: %c (use 'not' keyword instead)", ch))
			}
		case ch == '|':
			l.addToken(TokenUnion, "|")
		default:
			panic(fmt.Sprintf("unexpected character: %c", ch))
		}
	}
	l.addToken(TokenEOF, "")
	return l.tokens
}

func (l *Lexer) lexComment() {
	if l.input[l.pos] == '#' {
		l.pos++ // skip #
		// Single line comment
		for l.pos < len(l.input) && l.input[l.pos] != '\n' {
			l.pos++
		}
	}
}

func (l *Lexer) lexMultiLineComment() {
	// Called when we've seen (# already
	l.pos++ // skip #
	depth := 1
	for l.pos < len(l.input) {
		if l.pos+1 < len(l.input) {
			if l.input[l.pos] == '(' && l.input[l.pos+1] == '#' {
				depth++
				l.pos += 2
				continue
			} else if l.input[l.pos] == '#' && l.input[l.pos+1] == ')' {
				depth--
				l.pos += 2
				if depth == 0 {
					return
				}
				continue
			}
		}
		l.pos++
	}
}

func (l *Lexer) peekNextChar() byte {
	if l.pos+2 < len(l.input) {
		return l.input[l.pos+2]
	}
	return 0
}

func (l *Lexer) addToken(tt TokenType, val string) {
	l.tokens = append(l.tokens, Token{Type: tt, Value: val})
	l.pos += len(val)
}

func (l *Lexer) lexStringOrInterpolated() {
	l.pos++ // skip opening "
	start := l.pos
	hasInterpolation := false
	
	// First pass: check if it contains interpolation
	tempPos := l.pos
	for tempPos < len(l.input) && l.input[tempPos] != '"' {
		if tempPos+1 < len(l.input) && l.input[tempPos] == '#' && l.input[tempPos+1] == '{' {
			hasInterpolation = true
			break
		}
		tempPos++
	}
	
	if hasInterpolation {
		l.lexInterpolatedString('"')
	} else {
		// Simple string
		for l.pos < len(l.input) && l.input[l.pos] != '"' {
			l.pos++
		}
		str := l.input[start:l.pos]
		l.pos++ // skip closing "
		l.tokens = append(l.tokens, Token{Type: TokenString, Value: str})
	}
}

func (l *Lexer) lexMultiLineStringOrInterpolated() {
	l.pos += 2 // skip opening [[
	start := l.pos
	hasInterpolation := false
	
	// First pass: check if it contains interpolation
	tempPos := l.pos
	for tempPos < len(l.input)-1 {
		if l.input[tempPos] == ']' && l.input[tempPos+1] == ']' {
			break
		}
		if tempPos+1 < len(l.input) && l.input[tempPos] == '#' && l.input[tempPos+1] == '{' {
			hasInterpolation = true
			break
		}
		tempPos++
	}
	
	if hasInterpolation {
		l.lexInterpolatedString(']')
	} else {
		// Simple multi-line string
		for l.pos < len(l.input)-1 {
			if l.input[l.pos] == ']' && l.input[l.pos+1] == ']' {
				break
			}
			l.pos++
		}
		str := l.input[start:l.pos]
		l.pos += 2 // skip closing ]]
		l.tokens = append(l.tokens, Token{Type: TokenString, Value: str})
	}
}

func (l *Lexer) lexInterpolatedString(delimiter byte) {
	var parts []string
	start := l.pos
	
	for {
		// Check for end of string
		if delimiter == '"' && l.pos < len(l.input) && l.input[l.pos] == '"' {
			break
		}
		if delimiter == ']' && l.pos < len(l.input)-1 && l.input[l.pos] == ']' && l.input[l.pos+1] == ']' {
			break
		}
		
		// Check for interpolation start
		if l.pos+1 < len(l.input) && l.input[l.pos] == '#' && l.input[l.pos+1] == '{' {
			// Add text part before interpolation
			if l.pos > start {
				parts = append(parts, l.input[start:l.pos])
			}
			
			// Skip #{
			l.pos += 2
			
			// Find matching }
			braceDepth := 1
			exprStart := l.pos
			for l.pos < len(l.input) && braceDepth > 0 {
				if l.input[l.pos] == '{' {
					braceDepth++
				} else if l.input[l.pos] == '}' {
					braceDepth--
				}
				l.pos++
			}
			
			if braceDepth > 0 {
				panic("unterminated interpolation expression")
			}
			
			// Add expression part (without the closing })
			expr := l.input[exprStart : l.pos-1]
			parts = append(parts, "#{"+expr+"}")
			
			start = l.pos
		} else {
			l.pos++
		}
	}
	
	// Add remaining text
	if l.pos > start {
		parts = append(parts, l.input[start:l.pos])
	}
	
	// Skip closing delimiter
	if delimiter == '"' {
		l.pos++
	} else {
		l.pos += 2
	}
	
	// Join parts and create token
	fullString := ""
	for _, part := range parts {
		fullString += part
	}
	
	l.tokens = append(l.tokens, Token{Type: TokenInterpolatedString, Value: fullString})
}

func (l *Lexer) lexNumber() {
	start := l.pos
	hasDecimal := false
	
	// Parse digits
	for l.pos < len(l.input) && unicode.IsDigit(rune(l.input[l.pos])) {
		l.pos++
	}
	
	// Check for decimal point (but not if it's followed by another dot for ranges or letters for method calls)
	if l.pos < len(l.input) && l.input[l.pos] == '.' && l.peekChar() != '.' && unicode.IsDigit(rune(l.peekChar())) {
		hasDecimal = true
		l.pos++ // consume the '.'
		
		// Parse fractional part
		for l.pos < len(l.input) && unicode.IsDigit(rune(l.input[l.pos])) {
			l.pos++
		}
	}
	
	numStr := l.input[start:l.pos]
	if hasDecimal {
		l.tokens = append(l.tokens, Token{Type: TokenNumber, Value: numStr})
	} else {
		l.tokens = append(l.tokens, Token{Type: TokenNumber, Value: numStr})
	}
}

func (l *Lexer) peekChar() byte {
	if l.pos+1 < len(l.input) {
		return l.input[l.pos+1]
	}
	return 0
}

func (l *Lexer) lexIdentifier() {
	start := l.pos
	for l.pos < len(l.input) && (unicode.IsLetter(rune(l.input[l.pos])) || unicode.IsDigit(rune(l.input[l.pos])) || l.input[l.pos] == '-' || l.input[l.pos] == '_') {
		l.pos++
	}
	id := l.input[start:l.pos]
	if id == "fn" {
		l.tokens = append(l.tokens, Token{Type: TokenFunction, Value: id})
	} else if id == "end" {
		l.tokens = append(l.tokens, Token{Type: TokenEnd, Value: id})

	} else if id == "match" {
		l.tokens = append(l.tokens, Token{Type: TokenMatch, Value: id})
	} else if id == "then" {
		l.tokens = append(l.tokens, Token{Type: TokenThen, Value: id})
	} else if id == "true" {
		l.tokens = append(l.tokens, Token{Type: TokenTrue, Value: id})
	} else if id == "false" {
		l.tokens = append(l.tokens, Token{Type: TokenFalse, Value: id})
	} else if id == "record" {
		l.tokens = append(l.tokens, Token{Type: TokenRecord, Value: id})
	} else if id == "assert" {
		l.tokens = append(l.tokens, Token{Type: TokenAssert, Value: id})
	} else if id == "import" {
		l.tokens = append(l.tokens, Token{Type: TokenImport, Value: id})
	} else if id == "from" {
		l.tokens = append(l.tokens, Token{Type: TokenFrom, Value: id})
	} else if id == "type" {
		l.tokens = append(l.tokens, Token{Type: TokenTypeKeyword, Value: id})
	} else if id == "not" {
		l.tokens = append(l.tokens, Token{Type: TokenNot, Value: id})
	} else if id == "and" {
		l.tokens = append(l.tokens, Token{Type: TokenAnd, Value: id})
	} else if id == "or" {
		l.tokens = append(l.tokens, Token{Type: TokenOr, Value: id})
	} else if id == "file-type" {
		l.tokens = append(l.tokens, Token{Type: TokenTypeFile, Value: id})
	} else {
		l.tokens = append(l.tokens, Token{Type: TokenIdentifier, Value: id})
	}
}

// Parser
type Parser struct {
	tokens []Token
	pos    int
}

func NewParser(tokens []Token) *Parser {
	return &Parser{tokens: tokens, pos: 0}
}

func (p *Parser) Parse() []Stmt {
	var stmts []Stmt
	for p.current().Type != TokenEOF {
		stmts = append(stmts, p.parseStmt())
	}
	return stmts
}

func (p *Parser) parseStmt() Stmt {
	tok := p.current()
	if tok.Type == TokenImport {
		return p.parseImport()
	} else if tok.Type == TokenTypeKeyword {
		return p.parseTypeAlias()
	} else if tok.Type == TokenIdentifier && (p.peek().Type == TokenAssign || p.peek().Type == TokenColon || p.peek().Type == TokenWalrus) {
		return p.parseAssign()
	} else if tok.Type == TokenFunction {
		return p.parseFunction()
	} else if tok.Type == TokenMatch {
		return p.parseMatch()
	} else if tok.Type == TokenRecord {
		return p.parseRecordDecl()
	} else if tok.Type == TokenAssert {
		return p.parseAssert()
	} else {
		return &ExprStmt{Expr: p.parseExpr()}
	}
}

func (p *Parser) parseAssign() *AssignStmt {
	varName := p.consume(TokenIdentifier).Value
	var declType Type
	
	if p.current().Type == TokenColon {
		p.pos++
		declType = p.parseType()
		p.consume(TokenAssign)
	} else if p.current().Type == TokenWalrus {
		p.pos++
		// Type will be inferred from the expression
		declType = nil
	} else {
		p.consume(TokenAssign)
	}
	
	expr := p.parseExpr()
	return &AssignStmt{Var: varName, DeclType: declType, Expr: expr}
}

func (p *Parser) parseTypeAlias() *TypeAliasStmt {
	p.consume(TokenTypeKeyword)
	name := p.consume(TokenIdentifier).Value
	p.consume(TokenAssign)
	ty := p.parseType()
	return &TypeAliasStmt{Name: name, Ty: ty}
}

func (p *Parser) parseImport() *ImportStmt {
	p.consume(TokenImport)
	
	// Handle different import patterns:
	// import "./file.epos"
	// import function from "./file.epos"  
	// import {func1, func2} from "./file.epos"
	
	if p.current().Type == TokenString {
		// Simple wildcard import: import "./file.epos"
		path := p.consume(TokenString).Value
		return &ImportStmt{Path: path, IsWildcard: true}
	} else if p.current().Type == TokenLBrace {
		// Multi-import: import {func1, func2} from "./file.epos"
		p.consume(TokenLBrace)
		var items []string
		if p.current().Type != TokenRBrace {
			items = append(items, p.consume(TokenIdentifier).Value)
			for p.current().Type == TokenComma {
				p.pos++
				items = append(items, p.consume(TokenIdentifier).Value)
			}
		}
		p.consume(TokenRBrace)
		p.consume(TokenFrom)
		path := p.consume(TokenString).Value
		return &ImportStmt{Path: path, Items: items}
	} else {
		// Single import: import function from "./file.epos"
		alias := p.consume(TokenIdentifier).Value
		p.consume(TokenFrom)
		path := p.consume(TokenString).Value
		return &ImportStmt{Path: path, Alias: alias}
	}
}

func (p *Parser) parseFunction() *FunctionStmt {
	p.consume(TokenFunction)
	
	name := p.consume(TokenIdentifier).Value
	
	// Check for public visibility marker after function name
	isPublic := false
	if p.current().Type == TokenAsterisk {
		isPublic = true
		p.pos++
	}
	p.consume(TokenLParen)
	var params []Param
	if p.current().Type != TokenRParen {
		paramName := p.consume(TokenIdentifier).Value
		p.consume(TokenColon)
		paramType := p.parseType()
		var defaultVal Expr
		if p.current().Type == TokenAssign {
			p.pos++
			defaultVal = p.parseExpr()
		}
		params = append(params, Param{Name: paramName, Ty: paramType, Default: defaultVal})
		for p.current().Type == TokenComma {
			p.pos++
			paramName := p.consume(TokenIdentifier).Value
			p.consume(TokenColon)
			paramType := p.parseType()
			var defaultVal Expr
			if p.current().Type == TokenAssign {
				p.pos++
				defaultVal = p.parseExpr()
			}
			params = append(params, Param{Name: paramName, Ty: paramType, Default: defaultVal})
		}
	}
	p.consume(TokenRParen)
	var returnType Type = nil
	if p.current().Type == TokenColon {
		p.pos++
		returnType = p.parseType()
	}
	var body []Stmt
	for p.current().Type != TokenEnd {
		body = append(body, p.parseStmt())
	}
	p.consume(TokenEnd)
	// With implicit returns, the last statement should remain as is
	// The codegen will handle generating returns from the last expression
	return &FunctionStmt{Name: name, Params: params, ReturnType: returnType, Body: body, IsPublic: isPublic}
}

func (p *Parser) parseMatch() *MatchStmt {
	p.consume(TokenMatch)
	expr := p.parseExpr()
	p.consume(TokenThen)
	var cases []MatchCase
	var defaultStmt Stmt
	for p.current().Type != TokenEnd {
		if p.current().Type == TokenDefault {
			p.consume(TokenDefault)
			p.consume(TokenFatArrow)
			defaultStmt = p.parseStmt()
			break
		} else {
			var values []Expr
			values = append(values, p.parseExpr())
			for p.current().Type == TokenComma {
				p.pos++
				values = append(values, p.parseExpr())
			}
			p.consume(TokenFatArrow)
			body := p.parseStmt()
			cases = append(cases, MatchCase{Values: values, Body: body})
		}
	}
	p.consume(TokenEnd)
	return &MatchStmt{Expr: expr, Cases: cases, Default: defaultStmt}
}



func (p *Parser) parseAssert() *AssertStmt {
	p.consume(TokenAssert)
	condition := p.parseExpr()
	return &AssertStmt{Condition: condition}
}

func (p *Parser) parseRelational() Expr {
	expr := p.parseAdditive()
	for {
		tok := p.current()
		if tok.Type == TokenGT || tok.Type == TokenLT || tok.Type == TokenGTE || tok.Type == TokenLTE || tok.Type == TokenEQ || tok.Type == TokenNeq {
			p.pos++
			right := p.parseAdditive()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseExpr() Expr {
	return p.parseLogicalOr()
}

func (p *Parser) parseLogicalOr() Expr {
	expr := p.parseLogicalAnd()
	for {
		tok := p.current()
		if tok.Type == TokenOr {
			p.pos++
			right := p.parseLogicalAnd()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseLogicalAnd() Expr {
	expr := p.parseRelational()
	for {
		tok := p.current()
		if tok.Type == TokenAnd {
			p.pos++
			right := p.parseRelational()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseAdditive() Expr {
	expr := p.parseRange()
	for {
		tok := p.current()
		if tok.Type == TokenPlus || tok.Type == TokenMinus {
			p.pos++
			right := p.parseRange()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseRange() Expr {
	expr := p.parseMultiplicative()
	if p.current().Type == TokenSpread {
		p.pos++
		end := p.parseMultiplicative()
		expr = &RangeExpr{Start: expr, End: end}
	}
	return expr
}

func (p *Parser) parseMultiplicative() Expr {
	expr := p.parseUnary()
	for {
		tok := p.current()
		if tok.Type == TokenMul || tok.Type == TokenDiv || tok.Type == TokenMod {
			p.pos++
			right := p.parsePrimary()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseUnary() Expr {
	if p.current().Type == TokenMinus {
		p.pos++
		expr := p.parsePrimary()
		return &UnaryExpr{Op: TokenMinus, Expr: expr}
	}
	if p.current().Type == TokenNot {
		p.pos++
		expr := p.parsePrimary()
		return &UnaryExpr{Op: TokenNot, Expr: expr}
	}
	return p.parseDot()
}

func (p *Parser) parsePrimary() Expr {
	tok := p.current()
	var expr Expr
	switch tok.Type {
	case TokenFunction:
		// Lambda expression: fn(params): return_type => body
		p.pos++
		p.consume(TokenLParen)
		var params []Param
		if p.current().Type != TokenRParen {
			paramName := p.consume(TokenIdentifier).Value
			p.consume(TokenColon)
			paramType := p.parseType()
			params = append(params, Param{Name: paramName, Ty: paramType})
			for p.current().Type == TokenComma {
				p.pos++
				paramName := p.consume(TokenIdentifier).Value
				p.consume(TokenColon)
				paramType := p.parseType()
				params = append(params, Param{Name: paramName, Ty: paramType})
			}
		}
		p.consume(TokenRParen)
		
		var returnType Type = BasicType("void")
		if p.current().Type == TokenColon {
			p.pos++
			returnType = p.parseType()
		}
		
		p.consume(TokenFatArrow)
		body := p.parseExpr()
		
		expr = &LambdaExpr{
			Params: params,
			ReturnType: returnType,
			Body: body,
		}
	case TokenMatch:
		p.pos++
		matchExpr := p.parseExpr()
		p.consume(TokenThen)
		var cases []MatchCaseExpr
		var defaultExpr Expr
		for p.current().Type != TokenEnd {
			if p.current().Type == TokenDefault {
			p.consume(TokenDefault)
			p.consume(TokenFatArrow)
			defaultExpr = p.parseExpr()
			break
			} else {
			var values []Expr
			values = append(values, p.parseExpr())
			for p.current().Type == TokenComma {
			p.pos++
			values = append(values, p.parseExpr())
			}
			p.consume(TokenFatArrow)
			body := p.parseExpr()
			cases = append(cases, MatchCaseExpr{Values: values, Body: body})
			}
		}
		p.consume(TokenEnd)
		expr = &MatchExpr{Expr: matchExpr, Cases: cases, Default: defaultExpr}
	case TokenNumber:
		// Regular number parsing
		p.pos++
		// Check if it's a float (contains a decimal point)
		if strings.Contains(tok.Value, ".") {
			val, _ := strconv.ParseFloat(tok.Value, 64)
			expr = &FloatExpr{Value: val}
		} else {
			val, _ := strconv.ParseInt(tok.Value, 10, 64)
			expr = &NumberExpr{Value: val}
		}
	case TokenString:
		p.pos++
		expr = &StringExpr{Value: tok.Value}
	case TokenInterpolatedString:
		p.pos++
		expr = p.parseInterpolatedString(tok.Value)
	case TokenTrue:
		p.pos++
		expr = &BoolExpr{Value: true}
	case TokenFalse:
		p.pos++
		expr = &BoolExpr{Value: false}
	case TokenIdentifier:
		expr = &VarExpr{Name: tok.Value}
		p.pos++
	case TokenLParen:
		p.pos++
		expr = p.parseExpr()
		p.consume(TokenRParen)
	case TokenLBrace:
		p.pos++
		var elements []Expr
		if p.current().Type != TokenRBrace {
			elements = append(elements, p.parseListItem())
			for p.current().Type == TokenComma {
				p.pos++
				if p.current().Type != TokenRBrace {
					elements = append(elements, p.parseListItem())
				}
			}
		}
		p.consume(TokenRBrace)
		expr = &ListExpr{Elements: elements}
	case TokenAt:
		p.pos++
		p.consume(TokenLBrace)
		fields := make(map[string]Expr)
		for p.current().Type != TokenRBrace {
			field := p.consume(TokenIdentifier).Value
			// Only accept => for field assignment in record instances
			p.consume(TokenFatArrow)
			fieldExpr := p.parseExpr()
			fields[field] = fieldExpr
			if p.current().Type == TokenComma {
				p.pos++
			}
		}
		p.consume(TokenRBrace)
		expr = &RecordExpr{Fields: fields}
	case TokenSpread:
		// This should not happen in normal parsing, but if it does,
		// it indicates a parsing error where range parsing failed
		panic(fmt.Sprintf("unexpected range operator .. in expression context - this should be handled by parseRange()"))
	default:
		panic(fmt.Sprintf("unexpected token in primary: %v", tok))
	}

	for {
		if p.current().Type == TokenLParen {
			p.pos++
			var args []Expr
			if p.current().Type != TokenRParen {
				args = append(args, p.parseExpr())
				for p.current().Type == TokenComma {
					p.pos++
					args = append(args, p.parseExpr())
				}
			}
			p.consume(TokenRParen)
			expr = &CallExpr{Callee: expr, Args: args}
		} else if p.current().Type == TokenLBracket {
			p.pos++
			fieldTok := p.consume(TokenString)
			p.consume(TokenRBracket)
			expr = &FieldAccessExpr{Receiver: expr, Field: fieldTok.Value}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseType() Type {
	return p.parseUnionType()
}

func (p *Parser) parseUnionType() Type {
	t := p.parseSingleType()
	if p.current().Type == TokenUnion {
		var types []Type
		types = append(types, t)
		for p.current().Type == TokenUnion {
			p.pos++
			types = append(types, p.parseSingleType())
		}
		return UnionType{Types: types}
	}
	return t
}

func (p *Parser) parseSingleType() Type {
	if p.current().Type == TokenFunction {
		p.consume(TokenFunction)
		p.consume(TokenLParen)
		var params []Type
		if p.current().Type != TokenRParen {
			// Function type syntax: fn(int, string) -> bool
			// Only types, no parameter names
			params = append(params, p.parseType())
			for p.current().Type == TokenComma {
				p.pos++
				params = append(params, p.parseType())
			}
		}
		p.consume(TokenRParen)
		// Check if there's an arrow for return type, if not it's a void function
		var ret Type
		if p.current().Type == TokenArrow {
			p.consume(TokenArrow)
			ret = p.parseType()
		} else {
			ret = BasicType("void")
		}
		return FunctionType{Params: params, Return: ret}
	} else {
		tok := p.current()
		if tok.Type == TokenTypeFile {
			p.pos++
			return BasicType("file")
		}
		tok = p.consume(TokenIdentifier)
		switch tok.Value {
		case "int":
			return BasicType("int")
		case "string":
			return BasicType("string")
		case "bool":
			return BasicType("bool")
		case "float":
			return BasicType("float")
		case "file-type":
			return BasicType("file")
		case "void":
			return BasicType("void")
		case "list":
			p.consume(TokenLParen)
			elem := p.parseType()
			p.consume(TokenRParen)
			return ListType{Element: elem}
		default:
			// Check if it's a single lowercase letter (generic type)
			if len(tok.Value) == 1 && tok.Value[0] >= 'a' && tok.Value[0] <= 'z' {
				return GenericType{Name: tok.Value}
			}
			// Check if it's followed by parentheses for generic instantiation
			if p.current().Type == TokenLParen {
				p.consume(TokenLParen)
				var typeArgs []Type
				if p.current().Type != TokenRParen {
					typeArgs = append(typeArgs, p.parseType())
					for p.current().Type == TokenComma {
						p.consume(TokenComma)
						typeArgs = append(typeArgs, p.parseType())
					}
				}
				p.consume(TokenRParen)
				return GenericInstType{Name: tok.Value, TypeArgs: typeArgs}
			}
			return BasicType(tok.Value)
		}
	}
}

func (p *Parser) current() Token {
	return p.tokens[p.pos]
}

func (p *Parser) peek() Token {
	if p.pos+1 < len(p.tokens) {
		return p.tokens[p.pos+1]
	}
	return Token{Type: TokenEOF}
}

func (p *Parser) peekNext() Token {
	if p.pos+2 < len(p.tokens) {
		return p.tokens[p.pos+2]
	}
	return Token{Type: TokenEOF}
}

func (p *Parser) consume(tt TokenType) Token {
	tok := p.current()
	if tok.Type != tt {
		panic(fmt.Sprintf("expected token type %d, got %d with value '%s' at position %d", tt, tok.Type, tok.Value, p.pos))
	}
	p.pos++
	return tok
}

func (p *Parser) parseDot() Expr {
	expr := p.parsePrimary()
	for {
		tok := p.current()
		if tok.Type == TokenDot {
			p.pos++
			field := p.consume(TokenIdentifier).Value
			
			// Check if this is a uniform function call: expr.func(args)
			if p.current().Type == TokenLParen {
				p.pos++
				var args []Expr
				args = append(args, expr) // First argument is the receiver
				
				// Parse remaining arguments
				if p.current().Type != TokenRParen {
					args = append(args, p.parseExpr())
					for p.current().Type == TokenComma {
						p.pos++
						args = append(args, p.parseExpr())
					}
				}
				p.consume(TokenRParen)
				
				// Create a function call with the field name as the function
				expr = &CallExpr{
					Callee: &VarExpr{Name: field},
					Args:   args,
				}
			} else {
				// Regular field access
				expr = &FieldAccessExpr{Receiver: expr, Field: field}
			}
		} else if tok.Type == TokenLBracket {
			p.pos++
			fieldTok := p.consume(TokenString)
			p.consume(TokenRBracket)
			expr = &FieldAccessExpr{Receiver: expr, Field: fieldTok.Value}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseListItem() Expr {
	if p.current().Type == TokenSpread {
		p.pos++
		return &SpreadExpr{Expr: p.parseExpr()}
	}
	return p.parseExpr()
}

func (p *Parser) parseRecordDecl() *TypeAliasStmt {
	p.consume(TokenRecord)
	name := p.consume(TokenIdentifier).Value
	
	// Parse optional generic type parameters: record name(T, U, V)
	var typeParams []string
	if p.current().Type == TokenLParen {
		p.consume(TokenLParen)
		for p.current().Type != TokenRParen {
			typeParam := p.consume(TokenIdentifier).Value
			typeParams = append(typeParams, typeParam)
			if p.current().Type == TokenComma {
				p.consume(TokenComma)
			}
		}
		p.consume(TokenRParen)
	}
	
	var fields []Field
	for p.current().Type != TokenEnd {
		fieldName := p.consume(TokenIdentifier).Value
		optional := false
		if p.current().Type == TokenQuestion {
			p.consume(TokenQuestion)
			optional = true
		}
		p.consume(TokenColon)
		ty := p.parseType()
		fields = append(fields, Field{Name: fieldName, Ty: ty, Optional: optional})
	}
	p.consume(TokenEnd)
	ty := RecordType{Fields: fields, TypeParams: typeParams}
	return &TypeAliasStmt{Name: name, Ty: ty}
}

type TypeChecker struct {
	funcs map[string]struct {
		Params       []Type
		Return       Type
		RequiredArgs int  // number of required (non-default) parameters
	}
	namedTypes map[string]Type
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{funcs: make(map[string]struct {
		Params       []Type
		Return       Type
		RequiredArgs int
	}), namedTypes: make(map[string]Type)}
}

// GetNamedTypes returns the named types map
func (tc *TypeChecker) GetNamedTypes() map[string]Type {
	return tc.namedTypes
}

func (tc *TypeChecker) resolveType(t Type) Type {
	switch ty := t.(type) {
	case BasicType:
		if rt, ok := tc.namedTypes[string(ty)]; ok {
			return rt
		}
		return ty
	case ListType:
		return ListType{Element: tc.resolveType(ty.Element)}
	case RecordType:
		for i := range ty.Fields {
			ty.Fields[i].Ty = tc.resolveType(ty.Fields[i].Ty)
		}
		return ty
	case FunctionType:
		params := make([]Type, len(ty.Params))
		for i := range ty.Params {
			params[i] = tc.resolveType(ty.Params[i])
		}
		return FunctionType{Params: params, Return: tc.resolveType(ty.Return)}
	case GenericType:
		// Don't resolve generic types - they should remain as-is
		return ty
	case GenericInstType:
		// Convert GenericInstType to concrete RecordType
		if genericRecDef, exists := tc.namedTypes[ty.Name]; exists {
			if recType, ok := genericRecDef.(RecordType); ok && len(recType.TypeParams) > 0 {
				if len(ty.TypeArgs) == len(recType.TypeParams) {
					instFields := make([]Field, len(recType.Fields))
					for i, field := range recType.Fields {
						instFields[i] = Field{
							Name:     field.Name,
							Optional: field.Optional,
							Ty:       tc.instantiateGenericType(field.Ty, recType.TypeParams, ty.TypeArgs),
						}
					}
					return RecordType{Fields: instFields, TypeParams: nil}
				}
			}
		}
		// If not a generic record, just resolve the type arguments
		typeArgs := make([]Type, len(ty.TypeArgs))
		for i := range ty.TypeArgs {
			typeArgs[i] = tc.resolveType(ty.TypeArgs[i])
		}
		return GenericInstType{Name: ty.Name, TypeArgs: typeArgs}
	case UnionType:
		resolvedTypes := make([]Type, len(ty.Types))
		for i, t := range ty.Types {
			resolvedTypes[i] = tc.resolveType(t)
		}
		return UnionType{Types: resolvedTypes}
	default:
		return t
	}
}

func (p *Parser) parseInterpolatedString(value string) *InterpolatedStringExpr {
	var parts []StringPart
	i := 0
	
	for i < len(value) {
		// Find next interpolation
		start := i
		for i < len(value) {
			if i+1 < len(value) && value[i] == '#' && value[i+1] == '{' {
				break
			}
			i++
		}
		
		// Add text part if any
		if i > start {
			parts = append(parts, StringPart{
				IsExpr: false,
				Text:   value[start:i],
			})
		}
		
		// Parse interpolation if found
		if i < len(value) && i+1 < len(value) && value[i] == '#' && value[i+1] == '{' {
			i += 2 // skip #{
			exprStart := i
			braceDepth := 1
			
			// Find matching }
			for i < len(value) && braceDepth > 0 {
				if value[i] == '{' {
					braceDepth++
				} else if value[i] == '}' {
					braceDepth--
				}
				i++
			}
			
			if braceDepth > 0 {
				panic("unterminated interpolation expression")
			}
			
			// Parse the expression
			exprStr := value[exprStart : i-1]
			exprLexer := NewLexer(exprStr)
			exprTokens := exprLexer.Lex()
			exprParser := NewParser(exprTokens)
			expr := exprParser.parseExpr()
			
			parts = append(parts, StringPart{
				IsExpr: true,
				Expr:   expr,
			})
		}
	}
	
	return &InterpolatedStringExpr{Parts: parts}
}

func (tc *TypeChecker) TypeCheck(stmts []Stmt) error {
	// Collect function signatures
	for _, stmt := range stmts {
		if f, ok := stmt.(*FunctionStmt); ok {
			var params []Type
			requiredArgs := 0
			for _, p := range f.Params {
				params = append(params, p.Ty)
				if p.Default == nil {
					requiredArgs++
				}
			}
			returnType := f.ReturnType
			if returnType == nil {
				returnType = BasicType("void")
			}
			tc.funcs[f.Name] = struct {
				Params       []Type
				Return       Type
				RequiredArgs int
			}{Params: params, Return: returnType, RequiredArgs: requiredArgs}
		}
	}

	// Type check statements
	topEnv := make(map[string]Type)
	for _, stmt := range stmts {
		if err := tc.typeCheckStmt(stmt, topEnv); err != nil {
			return err
		}
	}
	return nil
}

func (tc *TypeChecker) typeCheckStmt(stmt Stmt, env map[string]Type) error {
	switch s := stmt.(type) {
	case *ImportStmt:
		return tc.processImport(s, env)
	case *TypeAliasStmt:
		resolvedTy := tc.resolveType(s.Ty)
		// Normalize record field ordering to be consistent with record literals
		if rt, ok := resolvedTy.(RecordType); ok {
			// Sort fields alphabetically to match record literal field ordering
			sort.Slice(rt.Fields, func(i, j int) bool {
				return rt.Fields[i].Name < rt.Fields[j].Name
			})
			resolvedTy = rt
		}
		tc.namedTypes[s.Name] = resolvedTy
		return nil
	case *FunctionStmt:
		localEnv := make(map[string]Type)
		for k, v := range env {
			localEnv[k] = v
		}
		for i, p := range s.Params {
			s.Params[i].Ty = tc.resolveType(p.Ty)
			localEnv[p.Name] = s.Params[i].Ty
		}
		s.ReturnType = tc.resolveType(s.ReturnType)
		for _, bodyStmt := range s.Body {
			if err := tc.typeCheckStmt(bodyStmt, localEnv); err != nil {
				return err
			}
		}
		// Check implicit return type from last statement
		if len(s.Body) > 0 && s.ReturnType != nil {
			lastStmt := s.Body[len(s.Body)-1]
			if exprStmt, ok := lastStmt.(*ExprStmt); ok {
				retTy, err := tc.typeCheckExpr(exprStmt.Expr, localEnv)
				if err != nil {
					return err
				}
				retTy = tc.resolveType(retTy)
				if !tc.compatible(retTy, s.ReturnType) {
					return fmt.Errorf("implicit return type mismatch: expected %v, got %v", s.ReturnType, retTy)
				}
				resolveTypes(exprStmt.Expr, s.ReturnType)
				exprStmt.Type = s.ReturnType
			}
		}
		return nil
	case *AssignStmt:
		ty, err := tc.typeCheckExpr(s.Expr, env)
		if err != nil {
			return err
		}
		ty = tc.resolveType(ty)
		if s.DeclType != nil {
			declType := tc.resolveType(s.DeclType)
			
			// Handle generic record instantiation
			if ginst, ok := declType.(GenericInstType); ok {
				// Look up the generic record definition
				if genericRecDef, exists := tc.namedTypes[ginst.Name]; exists {
					if recType, ok := genericRecDef.(RecordType); ok && len(recType.TypeParams) > 0 {
						// Create a mapping from generic type parameters to concrete types
						if len(ginst.TypeArgs) != len(recType.TypeParams) {
							return fmt.Errorf("wrong number of type arguments: expected %d, got %d", len(recType.TypeParams), len(ginst.TypeArgs))
						}
						
						// Create an instantiated record type
						instFields := make([]Field, len(recType.Fields))
						for i, field := range recType.Fields {
							instFields[i] = Field{
								Name:     field.Name,
								Optional: field.Optional,
								Ty:       tc.instantiateGenericType(field.Ty, recType.TypeParams, ginst.TypeArgs),
							}
						}
						declType = RecordType{Fields: instFields, TypeParams: nil}
					}
				}
			}
			
			if !tc.compatible(declType, ty) {
				return fmt.Errorf("type mismatch: expected %v, got %v", declType, ty)
			}
			resolveTypes(s.Expr, declType)
			ty = declType
		} else if isUnresolvedPlaceholder(ty) {
			return fmt.Errorf("cannot infer type for empty list without declaration")
		}
		if _, ok := env[s.Var]; ok {
			return fmt.Errorf("variable %s already defined (variables are immutable)", s.Var)
		}
		env[s.Var] = ty
		s.Type = ty
		return nil
	case *ExprStmt:
		ty, err := tc.typeCheckExpr(s.Expr, env)
		if err != nil {
			return err
		}
		s.Type = ty
		return nil
	case *MatchStmt:
		matchTy, err := tc.typeCheckExpr(s.Expr, env)
		if err != nil {
			return err
		}
		for _, cas := range s.Cases {
			for _, val := range cas.Values {
				valTy, err := tc.typeCheckExpr(val, env)
				if err != nil {
					return err
				}
				if !tc.equalTypes(valTy, matchTy) && !tc.compatible(matchTy, valTy) {
					return fmt.Errorf("case type mismatch")
				}
			}
			if err := tc.typeCheckStmt(cas.Body, env); err != nil {
				return err
			}
		}
		if s.Default != nil {
			if err := tc.typeCheckStmt(s.Default, env); err != nil {
				return err
			}
		}
		return nil

	case *AssertStmt:
		condTy, err := tc.typeCheckExpr(s.Condition, env)
		if err != nil {
			return err
		}
		if condTy != BasicType("bool") && condTy != BasicType("int") {
			return fmt.Errorf("assert condition must be bool or int, got %v", condTy)
		}
		s.Type = BasicType("void")
		return nil

	default:
		return fmt.Errorf("unsupported statement type")
	}
}

func (tc *TypeChecker) typeCheckExpr(expr Expr, env map[string]Type) (Type, error) {
	switch e := expr.(type) {
	case *NumberExpr:
		ty := BasicType("int")
		e.Type = ty
		return ty, nil
	case *FloatExpr:
		ty := BasicType("float")
		e.Type = ty
		return ty, nil
	case *BoolExpr:
		ty := BasicType("bool")
		e.Type = ty
		return ty, nil
	case *StringExpr:
		ty := BasicType("string")
		e.Type = ty
		return ty, nil
	case *InterpolatedStringExpr:
		// Type check all expression parts
		for i := range e.Parts {
			if e.Parts[i].IsExpr {
				_, err := tc.typeCheckExpr(e.Parts[i].Expr, env)
				if err != nil {
					return nil, err
				}
			}
		}
		ty := BasicType("string")
		e.Type = ty
		return ty, nil
	case *VarExpr:
		ty, ok := env[e.Name]
		if ok {
			e.Type = ty
			return ty, nil
		}
		sig, ok := tc.funcs[e.Name]
		if ok {
			ty := FunctionType{Params: sig.Params, Return: sig.Return}
			e.Type = ty
			return ty, nil
		}
		return nil, fmt.Errorf("undefined: %s", e.Name)
	case *UnaryExpr:
		ty, err := tc.typeCheckExpr(e.Expr, env)
		if err != nil {
			return nil, err
		}
		if e.Op == TokenNot {
			if ty != BasicType("bool") {
				return nil, fmt.Errorf("logical not operator requires boolean type")
			}
			e.Type = BasicType("bool")
			return BasicType("bool"), nil
		} else {
			if ty != BasicType("int") && ty != BasicType("float") {
				return nil, fmt.Errorf("unary operator requires numeric type")
			}
			e.Type = ty
			return ty, nil
		}
	case *BinaryExpr:
		leftTy, err := tc.typeCheckExpr(e.Left, env)
		if err != nil {
			return nil, err
		}
		rightTy, err := tc.typeCheckExpr(e.Right, env)
		if err != nil {
			return nil, err
		}

		if tc.equalTypes(leftTy, rightTy) {
			if leftTy == BasicType("int") && isNumberOp(e.Op) {
				e.Type = BasicType("int")
				return BasicType("int"), nil
			} else if leftTy == BasicType("float") && isNumberOp(e.Op) {
				e.Type = BasicType("float")
				return BasicType("float"), nil
			} else if isComparisonOp(e.Op) {
				e.Type = BasicType("bool")
				return BasicType("bool"), nil
			} else if leftTy == BasicType("bool") && isLogicalOp(e.Op) {
				e.Type = BasicType("bool")
				return BasicType("bool"), nil
			} else if leftTy == BasicType("string") && e.Op == TokenPlus {
				e.Type = BasicType("string")
				return BasicType("string"), nil
			} else if _, ok := leftTy.(GenericType); ok && isNumberOp(e.Op) {
				// For generic types, allow numeric operations and return the same generic type
				e.Type = leftTy
				return leftTy, nil
			} else if _, ok := leftTy.(GenericType); ok && e.Op == TokenPlus {
				// For generic types, allow + operation (could be string concat or numeric add)
				e.Type = leftTy
				return leftTy, nil
			} else if _, ok := leftTy.(GenericType); ok && isComparisonOp(e.Op) {
				// For generic types, allow comparison operations and return bool
				e.Type = BasicType("bool")
				return BasicType("bool"), nil
			}
		}
		return nil, fmt.Errorf("type mismatch in binary expression")
	case *CallExpr:
		if ve, ok := e.Callee.(*VarExpr); ok {
			if ve.Name == "print" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("print takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("void")
				return BasicType("void"), nil
			} else if ve.Name == "error" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("error takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if argTy != BasicType("string") {
					return nil, fmt.Errorf("error requires a string argument")
				}
				// The error function never returns (it exits), but we need a return type
				e.Type = BasicType("int") // dummy type since it never returns
				return BasicType("int"), nil
			} else if ve.Name == "compare" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("compare takes two arguments")
				}
				arg1Type, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				arg2Type, err := tc.typeCheckExpr(e.Args[1], env)
				if err != nil {
					return nil, err
				}
				// Both arguments must have the same type
				if !tc.equalTypes(arg1Type, arg2Type) {
					return nil, fmt.Errorf("compare requires both arguments to have the same type")
				}
				e.Type = BasicType("bool")
				return BasicType("bool"), nil
			} else if ve.Name == "elem" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("elem takes two arguments")
				}
				listTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				indexTy, err := tc.typeCheckExpr(e.Args[1], env)
				if err != nil {
					return nil, err
				}
				if indexTy != BasicType("int") {
					return nil, fmt.Errorf("index must be int")
				}
				if lt, ok := listTy.(ListType); ok {
					e.Type = lt.Element
					return lt.Element, nil
				}
				return nil, fmt.Errorf("elem called on non-list")
			} else if ve.Name == "len" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("len takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if lt, ok := argTy.(ListType); ok {
					_ = lt // unused
					e.Type = BasicType("int")
					return BasicType("int"), nil
				}
				if bt, ok := argTy.(BasicType); ok && bt == BasicType("string") {
					e.Type = BasicType("int")
					return BasicType("int"), nil
				}
				return nil, fmt.Errorf("len called on non-list or non-string")
			} else if ve.Name == "open-file" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("open-file takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if argTy != BasicType("string") {
					return nil, fmt.Errorf("open-file requires a string argument")
				}
				e.Type = BasicType("file")
				return BasicType("file"), nil
			} else if ve.Name == "write-file" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("write-file takes two arguments")
				}
				fileTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				contentTy, err := tc.typeCheckExpr(e.Args[1], env)
				if err != nil {
					return nil, err
				}
				if fileTy != BasicType("file") {
					return nil, fmt.Errorf("write-file requires a file as first argument")
				}
				if contentTy != BasicType("string") {
					return nil, fmt.Errorf("write-file requires a string as second argument")
				}
				e.Type = BasicType("int")
				return BasicType("int"), nil
			} else if ve.Name == "read-file" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("read-file takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if argTy != BasicType("file") {
					return nil, fmt.Errorf("read-file requires a file argument")
				}
				e.Type = BasicType("string")
				return BasicType("string"), nil
			} else if ve.Name == "close-file" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("close-file takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if argTy != BasicType("file") {
					return nil, fmt.Errorf("close-file requires a file argument")
				}
				e.Type = BasicType("int")
				return BasicType("int"), nil
			} else if ve.Name == "args" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("args takes no arguments")
				}
				e.Type = ListType{Element: BasicType("string")}
				return ListType{Element: BasicType("string")}, nil
			
			// LLVM Bindings
			} else if ve.Name == "llvm-module-create" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("llvm-module-create takes one argument")
				}
				argTy, err := tc.typeCheckExpr(e.Args[0], env)
				if err != nil {
					return nil, err
				}
				if argTy != BasicType("string") {
					return nil, fmt.Errorf("llvm-module-create requires a string argument")
				}
				e.Type = BasicType("llvm-module")
				return BasicType("llvm-module"), nil
			} else if ve.Name == "llvm-function-type" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-function-type takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // return type
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // param types list
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-add-function" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-add-function takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // module
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // name
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // function type
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-function")
				return BasicType("llvm-function"), nil
			} else if ve.Name == "llvm-append-basic-block" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-append-basic-block takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // function
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // name
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-basic-block")
				return BasicType("llvm-basic-block"), nil
			} else if ve.Name == "llvm-create-builder" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-create-builder takes no arguments")
				}
				e.Type = BasicType("llvm-builder")
				return BasicType("llvm-builder"), nil
			} else if ve.Name == "llvm-position-builder-at-end" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-position-builder-at-end takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // basic block
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("void")
				return BasicType("void"), nil
			} else if ve.Name == "llvm-build-global-string" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-global-string takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // string value
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // name
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-gep" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-gep takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // pointer
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // indices
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-call" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-call takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // function
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // args
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-ret" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-build-ret takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // value
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-const-int" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("llvm-const-int takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // value
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-type-i32" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-i32 takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-i8-ptr" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-i8-ptr takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-write-bitcode" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-write-bitcode takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // module
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // filename
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("void")
				return BasicType("void"), nil
			} else if ve.Name == "llvm-compile-to-executable" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-compile-to-executable takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // module
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // executable name
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("void")
				return BasicType("void"), nil
			// Additional LLVM type bindings
			} else if ve.Name == "llvm-type-i64" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-i64 takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-i8" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-i8 takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-i1" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-i1 takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-double" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-double takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-void" {
				if len(e.Args) != 0 {
					return nil, fmt.Errorf("llvm-type-void takes no arguments")
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-pointer" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("llvm-type-pointer takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // element type
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			} else if ve.Name == "llvm-type-struct" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("llvm-type-struct takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // field types list
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-type")
				return BasicType("llvm-type"), nil
			// More LLVM instruction bindings
			} else if ve.Name == "llvm-build-add" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-add takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // lhs
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // rhs
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-sub" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-sub takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // lhs
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // rhs
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-mul" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-mul takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // lhs
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // rhs
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-icmp" {
				if len(e.Args) != 4 {
					return nil, fmt.Errorf("llvm-build-icmp takes four arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // predicate
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // lhs
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[3], env) // rhs
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-alloca" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-build-alloca takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // type
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-store" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-store takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // value
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // pointer
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-load" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("llvm-build-load takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // type
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // pointer
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-br" {
				if len(e.Args) != 2 {
					return nil, fmt.Errorf("llvm-build-br takes two arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // basic block
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			} else if ve.Name == "llvm-build-cond-br" {
				if len(e.Args) != 4 {
					return nil, fmt.Errorf("llvm-build-cond-br takes four arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // builder
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // condition
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // then block
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[3], env) // else block
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("llvm-value")
				return BasicType("llvm-value"), nil
			// String manipulation functions for compiler
			} else if ve.Name == "string-length" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("string-length takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // string
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("int")
				return BasicType("int"), nil
			} else if ve.Name == "string-slice" {
				if len(e.Args) != 3 {
					return nil, fmt.Errorf("string-slice takes three arguments")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // string
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[1], env) // start
				if err != nil {
					return nil, err
				}
				_, err = tc.typeCheckExpr(e.Args[2], env) // end
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("string")
				return BasicType("string"), nil
			} else if ve.Name == "string-to-int" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("string-to-int takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // string
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("int")
				return BasicType("int"), nil
			} else if ve.Name == "int-to-string" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("int-to-string takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // int
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("string")
				return BasicType("string"), nil
			} else if ve.Name == "file-read-all" {
				if len(e.Args) != 1 {
					return nil, fmt.Errorf("file-read-all takes one argument")
				}
				_, err := tc.typeCheckExpr(e.Args[0], env) // filename
				if err != nil {
					return nil, err
				}
				e.Type = BasicType("string")
				return BasicType("string"), nil
			}
		}
		calleeTy, err := tc.typeCheckExpr(e.Callee, env)
		if err != nil {
			return nil, err
		}
		ft, ok := calleeTy.(FunctionType)
		if !ok {
			return nil, fmt.Errorf("called non-function of type %v", calleeTy)
		}
		// Get function signature to check for default parameters
		if ve, ok := e.Callee.(*VarExpr); ok {
			if sig, exists := tc.funcs[ve.Name]; exists {
				provided := len(e.Args)
				required := sig.RequiredArgs
				total := len(sig.Params)
				if provided < required || provided > total {
					return nil, fmt.Errorf("argument count mismatch: expected %d-%d, got %d", required, total, provided)
				}
			}
		} else if len(e.Args) != len(ft.Params) {
			return nil, fmt.Errorf("argument count mismatch: expected %d, got %d", len(ft.Params), len(e.Args))
		}
		
		// Handle generic type inference
		typeMap := make(map[string]Type)
		instantiatedParams := make([]Type, len(ft.Params))
		
		for i, arg := range e.Args {
			argTy, err := tc.typeCheckExpr(arg, env)
			if err != nil {
				return nil, err
			}
			
			// Resolve empty list/record types to match expected parameter types
			if isUnresolvedPlaceholder(argTy) {
				instantiatedParam := tc.instantiateWithMap(ft.Params[i], typeMap)
				resolveTypes(arg, instantiatedParam)
				argTy = instantiatedParam
			}
			
			// Try to unify the argument type with the parameter type
			instantiatedParam, err := tc.unifyTypes(ft.Params[i], argTy, typeMap)
			if err != nil {
				return nil, fmt.Errorf("argument %d type mismatch: %v", i, err)
			}
			instantiatedParams[i] = instantiatedParam
		}
		
		// Instantiate return type with the inferred type mappings
		instantiatedReturn := tc.instantiateWithMap(ft.Return, typeMap)
		
		e.Type = instantiatedReturn
		return instantiatedReturn, nil
	case *ListExpr:
		if len(e.Elements) == 0 {
			e.Type = ListType{Element: nil} // Placeholder for empty list
			return e.Type, nil
		}
		var elemTy Type
		for i, elem := range e.Elements {
			ty, err := tc.typeCheckExpr(elem, env)
			if err != nil {
				return nil, err
			}
			var currentElemTy Type
			if _, ok := elem.(*SpreadExpr); ok {
				if lt, ok := ty.(ListType); ok {
					// Skip empty spread lists (they contribute nothing to type inference)
					if isUnresolvedPlaceholder(lt) || lt.Element == nil {
						continue
					}
					currentElemTy = lt.Element
				} else {
					return nil, fmt.Errorf("spread on non-list")
				}
			} else {
				currentElemTy = ty
			}
			if i == 0 {
				elemTy = currentElemTy
			} else {
				if isPlaceholder(currentElemTy) {
					// ok
				} else if isPlaceholder(elemTy) {
					elemTy = currentElemTy
				} else if !tc.equalTypes(currentElemTy, elemTy) {
					return nil, fmt.Errorf("list elements have different types")
				}
			}
		}
		
		// If all elements were empty spreads, we need to infer the type from context
		if elemTy == nil {
			e.Type = ListType{Element: nil} // Placeholder like empty list
			return e.Type, nil
		}
		
		ty := ListType{Element: elemTy}
		e.Type = ty
		return ty, nil
	case *SpreadExpr:
		ty, err := tc.typeCheckExpr(e.Expr, env)
		if err != nil {
			return nil, err
		}
		ty = tc.resolveType(ty)
		if lt, ok := ty.(ListType); ok {
			e.Type = lt
			return lt, nil
		}
		return nil, fmt.Errorf("spread operator on non-list type")
	case *RangeExpr:
		startTy, err := tc.typeCheckExpr(e.Start, env)
		if err != nil {
			return nil, err
		}
		endTy, err := tc.typeCheckExpr(e.End, env)
		if err != nil {
			return nil, err
		}
		startTy = tc.resolveType(startTy)
		endTy = tc.resolveType(endTy)
		if startTy != BasicType("int") || endTy != BasicType("int") {
			return nil, fmt.Errorf("range expressions require integer bounds")
		}
		listTy := ListType{Element: BasicType("int")}
		e.Type = listTy
		return listTy, nil
	case *MatchExpr:
		matchTy, err := tc.typeCheckExpr(e.Expr, env)
		if err != nil {
			return nil, err
		}
		matchTy = tc.resolveType(matchTy)
		var resultTy Type
		for _, cas := range e.Cases {
			for _, val := range cas.Values {
				valTy, err := tc.typeCheckExpr(val, env)
				if err != nil {
					return nil, err
				}
				valTy = tc.resolveType(valTy)
				if !tc.equalTypes(valTy, matchTy) && !tc.compatible(matchTy, valTy) {
					return nil, fmt.Errorf("match case type mismatch")
				}
			}
			bodyTy, err := tc.typeCheckExpr(cas.Body, env)
			if err != nil {
				return nil, err
			}
			bodyTy = tc.resolveType(bodyTy)
			if resultTy == nil {
				resultTy = bodyTy
			} else if !tc.equalTypes(resultTy, bodyTy) {
				return nil, fmt.Errorf("match arms have different types")
			}
		}
		if e.Default != nil {
			defTy, err := tc.typeCheckExpr(e.Default, env)
			if err != nil {
				return nil, err
			}
			defTy = tc.resolveType(defTy)
			if resultTy == nil {
				resultTy = defTy
			} else if !tc.equalTypes(resultTy, defTy) {
				return nil, fmt.Errorf("match default type mismatch")
			}
		}
		if resultTy == nil {
			return nil, fmt.Errorf("match has no cases or default")
		}
		e.Type = resultTy
		return resultTy, nil
	case *RecordExpr:
		fieldTypes := make(map[string]Type)
		for name, ex := range e.Fields {
			fty, err := tc.typeCheckExpr(ex, env)
			if err != nil {
				return nil, err
			}
			fieldTypes[name] = tc.resolveType(fty)
		}
		var keys []string
		for k := range fieldTypes {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		var fields []Field
		for _, k := range keys {
			fields = append(fields, Field{Name: k, Ty: fieldTypes[k], Optional: false})
		}
		ty := RecordType{Fields: fields}
		e.Type = ty
		return ty, nil
	case *FieldAccessExpr:
		recTy, err := tc.typeCheckExpr(e.Receiver, env)
		if err != nil {
			return nil, err
		}
		recTy = tc.resolveType(recTy)
		rt, ok := recTy.(RecordType)
		if !ok {
			return nil, fmt.Errorf("field access on non-record")
		}
		for _, f := range rt.Fields {
			if f.Name == e.Field {
				e.Type = f.Ty
				return f.Ty, nil
			}
		}
		return nil, fmt.Errorf("field %s not found in record", e.Field)
	case *PipeExpr:
		// For pipe expressions: left |> right
		// The left side is the input, and right side should be a function or callable expression
		_, err := tc.typeCheckExpr(e.Left, env)
		if err != nil {
			return nil, err
		}
		
		// The right side should be a function call where we need to insert the left as first argument
		if callExpr, ok := e.Right.(*CallExpr); ok {
			// Create new args with left value as first argument
			newArgs := append([]Expr{e.Left}, callExpr.Args...)
			newCall := &CallExpr{Callee: callExpr.Callee, Args: newArgs}
			
			// Type check the modified call
			resultTy, err := tc.typeCheckExpr(newCall, env)
			if err != nil {
				return nil, err
			}
			e.Type = resultTy
			return resultTy, nil
		} else {
			return nil, fmt.Errorf("right side of pipe must be a function call")
		}
	case *LambdaExpr:
		// Create local environment for lambda parameters
		lambdaEnv := make(map[string]Type)
		for k, v := range env {
			lambdaEnv[k] = v
		}
		
		// Add parameters to lambda environment
		var paramTypes []Type
		for _, param := range e.Params {
			paramTypes = append(paramTypes, param.Ty)
			lambdaEnv[param.Name] = param.Ty
		}
		
		// Type check lambda body
		bodyTy, err := tc.typeCheckExpr(e.Body, lambdaEnv)
		if err != nil {
			return nil, err
		}
		
		// Check if body type matches return type
		if e.ReturnType != nil && !tc.equalTypes(bodyTy, e.ReturnType) {
			return nil, fmt.Errorf("lambda body type %v does not match declared return type %v", bodyTy, e.ReturnType)
		}
		
		// If no return type declared, infer from body
		if e.ReturnType == nil {
			e.ReturnType = bodyTy
		}
		
		// Create function type for lambda
		funcTy := FunctionType{
			Params: paramTypes,
			Return: e.ReturnType,
		}
		e.Type = funcTy
		return funcTy, nil
	default:
		return nil, fmt.Errorf("unsupported expression type")
	}
}

func (tc *TypeChecker) instantiateGenericType(ty Type, typeParams []string, typeArgs []Type) Type {
	switch t := ty.(type) {
	case GenericType:
		// Replace generic type parameter with concrete type
		for i, param := range typeParams {
			if t.Name == param {
				return typeArgs[i]
			}
		}
		return t
	case GenericInstType:
		// Instantiate the type arguments of the generic instantiation
		instArgs := make([]Type, len(t.TypeArgs))
		for i, arg := range t.TypeArgs {
			instArgs[i] = tc.instantiateGenericType(arg, typeParams, typeArgs)
		}
		return GenericInstType{Name: t.Name, TypeArgs: instArgs}
	case ListType:
		return ListType{Element: tc.instantiateGenericType(t.Element, typeParams, typeArgs)}
	case RecordType:
		instFields := make([]Field, len(t.Fields))
		for i, field := range t.Fields {
			instFields[i] = Field{
				Name:     field.Name,
				Optional: field.Optional,
				Ty:       tc.instantiateGenericType(field.Ty, typeParams, typeArgs),
			}
		}
		return RecordType{Fields: instFields, TypeParams: t.TypeParams}
	case FunctionType:
		instParams := make([]Type, len(t.Params))
		for i, param := range t.Params {
			instParams[i] = tc.instantiateGenericType(param, typeParams, typeArgs)
		}
		return FunctionType{
			Params: instParams,
			Return: tc.instantiateGenericType(t.Return, typeParams, typeArgs),
		}
	default:
		return ty
	}
}

func (tc *TypeChecker) equalTypes(a, b Type) bool {
	a = tc.resolveType(a)
	b = tc.resolveType(b)
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	switch a1 := a.(type) {
	case BasicType:
		if b1, ok := b.(BasicType); ok {
			return a1 == b1
		}
	case ListType:
		if b1, ok := b.(ListType); ok {
			return tc.equalTypes(a1.Element, b1.Element)
		}
	case RecordType:
		if b1, ok := b.(RecordType); ok {
			if len(a1.Fields) != len(b1.Fields) {
				return false
			}
			for i := range a1.Fields {
				if a1.Fields[i].Name != b1.Fields[i].Name ||
					!tc.equalTypes(a1.Fields[i].Ty, b1.Fields[i].Ty) ||
					a1.Fields[i].Optional != b1.Fields[i].Optional {
					return false
				}
			}
			return true
		}
	case FunctionType:
		if b1, ok := b.(FunctionType); ok {
			if len(a1.Params) != len(b1.Params) {
				return false
			}
			for i := range a1.Params {
				if !tc.equalTypes(a1.Params[i], b1.Params[i]) {
					return false
				}
			}
			return tc.equalTypes(a1.Return, b1.Return)
		}
	case GenericType:
		if b1, ok := b.(GenericType); ok {
			return a1.Name == b1.Name
		}
	case GenericInstType:
		if b1, ok := b.(GenericInstType); ok {
			if a1.Name != b1.Name || len(a1.TypeArgs) != len(b1.TypeArgs) {
				return false
			}
			for i := range a1.TypeArgs {
				if !tc.equalTypes(a1.TypeArgs[i], b1.TypeArgs[i]) {
					return false
				}
			}
			return true
		}
	case UnionType:
		if b1, ok := b.(UnionType); ok {
			if len(a1.Types) != len(b1.Types) {
				return false
			}
			// Check if all types in a1 are present in b1 (order doesn't matter)
			for _, aType := range a1.Types {
				found := false
				for _, bType := range b1.Types {
					if tc.equalTypes(aType, bType) {
						found = true
						break
					}
				}
				if !found {
					return false
				}
			}
			return true
		}
	}
	return false
}

// structurallyCompatible checks if two types are structurally compatible,
// especially for named record types vs inferred record types with same fields
func (tc *TypeChecker) structurallyCompatible(expected, actual Type) bool {
	expected = tc.resolveType(expected)
	actual = tc.resolveType(actual)
	
	// Handle named types by looking up their definitions
	if expectedBasic, ok := expected.(BasicType); ok {
		if expectedDef, exists := tc.namedTypes[string(expectedBasic)]; exists {
			expected = expectedDef
		}
	}
	if actualBasic, ok := actual.(BasicType); ok {
		if actualDef, exists := tc.namedTypes[string(actualBasic)]; exists {
			actual = actualDef
		}
	}
	
	// Check structural compatibility for records
	expectedRec, expectedIsRec := expected.(RecordType)
	actualRec, actualIsRec := actual.(RecordType)
	
	if expectedIsRec && actualIsRec {
		// Both are record types - check if they have the same fields
		if len(expectedRec.Fields) != len(actualRec.Fields) {
			return false
		}
		
		// Create field maps for comparison (order doesn't matter for structural typing)
		expectedFields := make(map[string]Field)
		for _, field := range expectedRec.Fields {
			expectedFields[field.Name] = field
		}
		
		// Check if all actual fields match expected fields
		for _, actualField := range actualRec.Fields {
			expectedField, exists := expectedFields[actualField.Name]
			if !exists {
				return false
			}
			if actualField.Optional != expectedField.Optional ||
				!tc.equalTypes(actualField.Ty, expectedField.Ty) {
				return false
			}
		}
		return true
	}
	
	return false
}

// resolveGenericType follows type variable mappings to find the concrete type
func (tc *TypeChecker) resolveGenericType(t Type, typeMap map[string]Type) Type {
	visited := make(map[string]bool)
	for {
		if generic, ok := t.(GenericType); ok {
			if visited[generic.Name] {
				// Cycle detected, return the generic type as-is
				return t
			}
			if mapped, exists := typeMap[generic.Name]; exists {
				visited[generic.Name] = true
				t = mapped
				continue
			}
		}
		return t
	}
}

// Unify two types, handling generic type variables
func (tc *TypeChecker) unifyTypes(expected, actual Type, typeMap map[string]Type) (Type, error) {
	switch expectedType := expected.(type) {
	case GenericType:
		// Resolve the expected type to its concrete type if already mapped
		resolvedExpected := tc.resolveGenericType(expected, typeMap)
		if resolvedExpected != expected {
			// Expected type resolves to something else, only continue if it's not a generic
			if _, isGeneric := resolvedExpected.(GenericType); !isGeneric {
				return tc.unifyTypes(resolvedExpected, actual, typeMap)
			}
		}
		
		// If actual is also a generic type, resolve it too
		if _, ok := actual.(GenericType); ok {
			resolvedActual := tc.resolveGenericType(actual, typeMap)
			if resolvedActual != actual {
				// Actual type resolves to something else, only continue if it's not a generic
				if _, isGeneric := resolvedActual.(GenericType); !isGeneric {
					return tc.unifyTypes(expected, resolvedActual, typeMap)
				}
			}
			// Both are generic types - map them to each other if not already mapped
			if _, exists := typeMap[expectedType.Name]; !exists {
				typeMap[expectedType.Name] = actual
			}
			return actual, nil
		}
		
		// Map the generic type to the actual concrete type
		typeMap[expectedType.Name] = actual
		return actual, nil
	case ListType:
		if actualList, ok := actual.(ListType); ok {
			unifiedElement, err := tc.unifyTypes(expectedType.Element, actualList.Element, typeMap)
			if err != nil {
				return nil, err
			}
			return ListType{Element: unifiedElement}, nil
		}
		return nil, fmt.Errorf("expected list type, got %v", actual)
	case FunctionType:
		if actualFunc, ok := actual.(FunctionType); ok {
			if len(expectedType.Params) != len(actualFunc.Params) {
				return nil, fmt.Errorf("function parameter count mismatch")
			}
			unifiedParams := make([]Type, len(expectedType.Params))
			for i := range expectedType.Params {
				unified, err := tc.unifyTypes(expectedType.Params[i], actualFunc.Params[i], typeMap)
				if err != nil {
					return nil, err
				}
				unifiedParams[i] = unified
			}
			unifiedReturn, err := tc.unifyTypes(expectedType.Return, actualFunc.Return, typeMap)
			if err != nil {
				return nil, err
			}
			return FunctionType{Params: unifiedParams, Return: unifiedReturn}, nil
		}
		return nil, fmt.Errorf("expected function type, got %v", actual)
	case UnionType:
		// Check if actual type is compatible with any type in the union
		for _, unionType := range expectedType.Types {
			if _, err := tc.unifyTypes(unionType, actual, typeMap); err == nil {
				return expected, nil // Return the union type, not the specific unified type
			}
		}
		return nil, fmt.Errorf("type %v is not compatible with union %v", actual, expected)
	default:
		// For concrete types, they must match exactly or be compatible
		if tc.equalTypes(expected, actual) {
			return actual, nil
		}
		
		// Check for structural compatibility between named record types and inferred records
		if tc.structurallyCompatible(expected, actual) {
			return expected, nil // Return the expected type to maintain named type identity
		}
		
		// Check if it can be unified with a union type
		if unionType, ok := expected.(UnionType); ok {
			for _, uType := range unionType.Types {
				if _, err := tc.unifyTypes(uType, actual, typeMap); err == nil {
					return expected, nil
				}
			}
		}
		return nil, fmt.Errorf("expected %v, got %v", expected, actual)
	}
}

func (tc *TypeChecker) instantiateWithMap(t Type, typeMap map[string]Type) Type {
	typeParams := make([]string, 0, len(typeMap))
	typeArgs := make([]Type, 0, len(typeMap))
	for param, arg := range typeMap {
		typeParams = append(typeParams, param)
		typeArgs = append(typeArgs, arg)
	}
	return tc.instantiateGenericType(t, typeParams, typeArgs)
}


func (tc *TypeChecker) compatible(a, b Type) bool {
	a = tc.resolveType(a)
	b = tc.resolveType(b)
	
	// Convert GenericInstType to concrete RecordType for both sides
	if ginst, ok := a.(GenericInstType); ok {
		if genericRecDef, exists := tc.namedTypes[ginst.Name]; exists {
			if recType, ok := genericRecDef.(RecordType); ok && len(recType.TypeParams) > 0 {
				if len(ginst.TypeArgs) == len(recType.TypeParams) {
					instFields := make([]Field, len(recType.Fields))
					for i, field := range recType.Fields {
						instFields[i] = Field{
							Name:     field.Name,
							Optional: field.Optional,
							Ty:       tc.instantiateGenericType(field.Ty, recType.TypeParams, ginst.TypeArgs),
						}
					}
					a = RecordType{Fields: instFields, TypeParams: nil}
				}
			}
		}
	}
	if ginst, ok := b.(GenericInstType); ok {
		if genericRecDef, exists := tc.namedTypes[ginst.Name]; exists {
			if recType, ok := genericRecDef.(RecordType); ok && len(recType.TypeParams) > 0 {
				if len(ginst.TypeArgs) == len(recType.TypeParams) {
					instFields := make([]Field, len(recType.Fields))
					for i, field := range recType.Fields {
						instFields[i] = Field{
							Name:     field.Name,
							Optional: field.Optional,
							Ty:       tc.instantiateGenericType(field.Ty, recType.TypeParams, ginst.TypeArgs),
						}
					}
					b = RecordType{Fields: instFields, TypeParams: nil}
				}
			}
		}
	}
	if a == nil {
		return true
	}
	if b == nil {
		// Allow generic types to be compatible with nil (unresolved placeholders)
		if _, ok := a.(GenericType); ok {
			return true
		}
		return false
	}
	switch a1 := a.(type) {
	case BasicType:
		b1, ok := b.(BasicType)
		return ok && a1 == b1
	case ListType:
		b1, ok := b.(ListType)
		return ok && tc.compatible(a1.Element, b1.Element)
	case RecordType:
		b1, ok := b.(RecordType)
		if !ok {
			return false
		}
		fieldMapA := make(map[string]Type)
		for _, f := range a1.Fields {
			fieldMapA[f.Name] = f.Ty
		}
		for _, f := range b1.Fields {
			ftyA, ok := fieldMapA[f.Name]
			if !ok {
				if !f.Optional {
					return false
				}
				continue
			}
			if !tc.compatible(ftyA, f.Ty) {
				return false
			}
		}
		return true
	case FunctionType:
		b1, ok := b.(FunctionType)
		if !ok {
			return false
		}
		if len(a1.Params) != len(b1.Params) {
			return false
		}
		for i := range a1.Params {
			if !tc.compatible(a1.Params[i], b1.Params[i]) {
				return false
			}
		}
		return tc.compatible(a1.Return, b1.Return)
	case GenericType:
		b1, ok := b.(GenericType)
		return ok && a1.Name == b1.Name
	case UnionType:
		// Check if b is compatible with any type in the union
		for _, unionType := range a1.Types {
			if tc.compatible(unionType, b) {
				return true
			}
		}
		return false
	default:
		// Check if a is assignable to a union type b
		if unionType, ok := b.(UnionType); ok {
			for _, bType := range unionType.Types {
				if tc.compatible(a, bType) {
					return true
				}
			}
			return false
		}
		return false
	}
}

func isPlaceholder(t Type) bool {
	if t == nil {
		return true
	}
	if lt, ok := t.(ListType); ok {
		return lt.Element == nil
	}
	return false
}

func isUnresolvedPlaceholder(t Type) bool {
	if t == nil {
		return true
	}
	if lt, ok := t.(ListType); ok {
		if lt.Element == nil {
			return true
		}
		return isUnresolvedPlaceholder(lt.Element)
	}
	return false
}

func resolveTypes(expr Expr, ty Type) {
	if le, ok := expr.(*ListExpr); ok {
		le.Type = ty
		if lt, ok := ty.(ListType); ok {
			for _, elem := range le.Elements {
				resolveTypes(elem, lt.Element)
			}
		}
	} else if re, ok := expr.(*RecordExpr); ok {
		re.Type = ty
		if rt, ok := ty.(RecordType); ok {
			for _, f := range rt.Fields {
				if fieldExpr, ok := re.Fields[f.Name]; ok {
					resolveTypes(fieldExpr, f.Ty)
				}
			}
		}
	} else if ie, ok := expr.(*InterpolatedStringExpr); ok {
		ie.Type = ty
		for _, part := range ie.Parts {
			if part.IsExpr {
				// For interpolated expressions, we don't force a specific type
				// They will be converted to string during codegen
			}
		}
	}
}

func isNumberOp(op TokenType) bool {
	switch op {
	case TokenPlus, TokenMinus, TokenMul, TokenDiv, TokenMod:
		return true
	default:
		return false
	}
}

func isComparisonOp(op TokenType) bool {
	switch op {
	case TokenGT, TokenLT, TokenGTE, TokenLTE, TokenEQ, TokenNeq:
		return true
	default:
		return false
	}
}

func isLogicalOp(op TokenType) bool {
	switch op {
	case TokenAnd, TokenOr:
		return true
	default:
		return false
	}
}

func (tc *TypeChecker) processImport(stmt *ImportStmt, env map[string]Type) error {
	// Resolve the import path relative to current working directory
	importPath := stmt.Path
	if !filepath.IsAbs(importPath) {
		var err error
		importPath, err = filepath.Abs(importPath)
		if err != nil {
			return fmt.Errorf("failed to resolve import path %s: %v", stmt.Path, err)
		}
	}

	// Read the imported file
	code, err := os.ReadFile(importPath)
	if err != nil {
		return fmt.Errorf("failed to read import file %s: %v", importPath, err)
	}

	// Parse the imported file
	lexer := NewLexer(string(code))
	tokens := lexer.Lex()
	parser := NewParser(tokens)
	importedStmts := parser.Parse()

	// Type check the imported file with empty environment
	importTC := NewTypeChecker()
	importEnv := make(map[string]Type)
	for _, s := range importedStmts {
		if err := importTC.typeCheckStmt(s, importEnv); err != nil {
			return fmt.Errorf("type error in imported file %s: %v", importPath, err)
		}
	}

	// Add imported functions to current environment based on import type
	if stmt.IsWildcard {
		// Import all public functions
		for _, s := range importedStmts {
			if funcStmt, ok := s.(*FunctionStmt); ok && funcStmt.IsPublic {
				env[funcStmt.Name] = FunctionType{
					Params: getFunctionParamTypes(funcStmt),
					Return: funcStmt.ReturnType,
				}
			}
		}
	} else if len(stmt.Items) > 0 {
		// Import specific functions
		for _, item := range stmt.Items {
			found := false
			for _, s := range importedStmts {
				if funcStmt, ok := s.(*FunctionStmt); ok && funcStmt.Name == item {
					env[item] = FunctionType{
						Params: getFunctionParamTypes(funcStmt),
						Return: funcStmt.ReturnType,
					}
					found = true
					break
				}
			}
			if !found {
				return fmt.Errorf("function %s not found in imported file %s", item, importPath)
			}
		}
	} else if stmt.Alias != "" {
		// Import single function with alias
		found := false
		for _, s := range importedStmts {
			if funcStmt, ok := s.(*FunctionStmt); ok && funcStmt.Name == stmt.Alias {
				env[stmt.Alias] = FunctionType{
					Params: getFunctionParamTypes(funcStmt),
					Return: funcStmt.ReturnType,
				}
				found = true
				break
			}
		}
		if !found {
			return fmt.Errorf("function %s not found in imported file %s", stmt.Alias, importPath)
		}
	}

	return nil
}

func getFunctionParamTypes(funcStmt *FunctionStmt) []Type {
	var paramTypes []Type
	for _, param := range funcStmt.Params {
		paramTypes = append(paramTypes, param.Ty)
	}
	return paramTypes
}
