package parser

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
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
	TokenAssign

	TokenSemicolon
	TokenLParen
	TokenRParen
	TokenFunction
	TokenEnd
	TokenReturn
	TokenComma
	TokenIf
	TokenThen
	TokenElse
	TokenGT
	TokenLT
	TokenEQ
	TokenWhile
	TokenDo
	TokenString
	TokenMatch
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

type ReturnStmt struct {
	Expr Expr
	Type Type
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
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
	Fields []Field
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

type RecordExpr struct {
	Fields map[string]Expr
	Type   Type
}

type FieldAccessExpr struct {
	Receiver Expr
	Field    string
	Type     Type
}

type TypeValueExpr struct {
	Ty   Type
	Type Type
}

type RecordDecl struct {
	Name   string
	Fields []Field
}

type Param struct {
	Name string
	Ty   Type
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
			l.addToken(TokenGT, ">")
		case ch == '<':
			l.addToken(TokenLT, "<")
		case ch == ',':
			l.addToken(TokenComma, ",")
		case ch == ';':
			l.addToken(TokenSemicolon, ";")
		case ch == '_':
			l.addToken(TokenDefault, "_")
		case ch == '(':
			l.addToken(TokenLParen, "(")
		case ch == ')':
			l.addToken(TokenRParen, ")")
		case ch == ']':
			l.addToken(TokenRBracket, "]")
		case ch == '{':
			l.addToken(TokenLBrace, "{")
		case ch == '}':
			l.addToken(TokenRBrace, "}")
		case ch == ':':
			l.addToken(TokenColon, ":")
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
		default:
			panic(fmt.Sprintf("unexpected character: %c", ch))
		}
	}
	l.addToken(TokenEOF, "")
	return l.tokens
}

func (l *Lexer) lexComment() {
	l.pos++ // skip #
	if l.pos < len(l.input) && l.input[l.pos] == '[' {
		l.pos++ // skip [
		depth := 1
		for l.pos < len(l.input) {
			if l.pos+1 < len(l.input) {
				if l.input[l.pos] == '#' && l.input[l.pos+1] == '[' {
					depth++
					l.pos += 2
					continue
				} else if l.input[l.pos] == ']' && l.input[l.pos+1] == '#' {
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
	} else {
		for l.pos < len(l.input) && l.input[l.pos] != '\n' {
			l.pos++
		}
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
	for l.pos < len(l.input) && unicode.IsDigit(rune(l.input[l.pos])) {
		l.pos++
	}
	numStr := l.input[start:l.pos]
	l.tokens = append(l.tokens, Token{Type: TokenNumber, Value: numStr})
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
	} else if id == "return" {
		l.tokens = append(l.tokens, Token{Type: TokenReturn, Value: id})
	} else if id == "if" {
		l.tokens = append(l.tokens, Token{Type: TokenIf, Value: id})
	} else if id == "then" {
		l.tokens = append(l.tokens, Token{Type: TokenThen, Value: id})
	} else if id == "else" {
		l.tokens = append(l.tokens, Token{Type: TokenElse, Value: id})
	} else if id == "while" {
		l.tokens = append(l.tokens, Token{Type: TokenWhile, Value: id})
	} else if id == "do" {
		l.tokens = append(l.tokens, Token{Type: TokenDo, Value: id})
	} else if id == "match" {
		l.tokens = append(l.tokens, Token{Type: TokenMatch, Value: id})
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
	} else if tok.Type == TokenIdentifier && (p.peek().Type == TokenAssign || p.peek().Type == TokenColon) {
		return p.parseAssign()
	} else if tok.Type == TokenFunction {
		return p.parseFunction()
	} else if tok.Type == TokenReturn {
		return p.parseReturn()
	} else if tok.Type == TokenIf {
		return p.parseIf()
	} else if tok.Type == TokenWhile {
		return p.parseWhile()
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
	}
	p.consume(TokenAssign)
	var expr Expr
	if declType != nil {
		if bt, ok := declType.(BasicType); ok && string(bt) == "type" {
			expr = &TypeValueExpr{Ty: p.parseType()}
		} else {
			expr = p.parseExpr()
		}
	} else {
		expr = p.parseExpr()
	}
	return &AssignStmt{Var: varName, DeclType: declType, Expr: expr}
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
	if len(body) > 0 && returnType != nil {
		lastStmt := body[len(body)-1]
		if exprStmt, ok := lastStmt.(*ExprStmt); ok {
			body[len(body)-1] = &ReturnStmt{Expr: exprStmt.Expr}
		} else if matchStmt, ok := lastStmt.(*MatchStmt); ok {
			// Convert MatchStmt to MatchExpr wrapped in ReturnStmt
			var matchCases []MatchCaseExpr
			for _, c := range matchStmt.Cases {
				if exprStmt, ok := c.Body.(*ExprStmt); ok {
					matchCases = append(matchCases, MatchCaseExpr{Values: c.Values, Body: exprStmt.Expr})
				} else {
					panic("match case body must be an expression for implicit return")
				}
			}
			var defaultExpr Expr
			if matchStmt.Default != nil {
				if exprStmt, ok := matchStmt.Default.(*ExprStmt); ok {
					defaultExpr = exprStmt.Expr
				} else {
					panic("match default body must be an expression for implicit return")
				}
			}
			matchExpr := &MatchExpr{Expr: matchStmt.Expr, Cases: matchCases, Default: defaultExpr}
			body[len(body)-1] = &ReturnStmt{Expr: matchExpr}
		}
	}
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
			p.consume(TokenArrow)
			defaultStmt = p.parseStmt()
			break
		} else {
			var values []Expr
			values = append(values, p.parseExpr())
			for p.current().Type == TokenComma {
				p.pos++
				values = append(values, p.parseExpr())
			}
			p.consume(TokenArrow)
			body := p.parseStmt()
			cases = append(cases, MatchCase{Values: values, Body: body})
		}
	}
	p.consume(TokenEnd)
	return &MatchStmt{Expr: expr, Cases: cases, Default: defaultStmt}
}

func (p *Parser) parseWhile() *WhileStmt {
	p.consume(TokenWhile)
	cond := p.parseExpr()
	p.consume(TokenDo)
	var body []Stmt
	for p.current().Type != TokenEnd {
		body = append(body, p.parseStmt())
	}
	p.consume(TokenEnd)
	return &WhileStmt{Cond: cond, Body: body}
}

func (p *Parser) parseIf() *IfStmt {
	p.consume(TokenIf)
	cond := p.parseExpr()
	p.consume(TokenThen)
	var thenBody []Stmt
	for p.current().Type != TokenElse && p.current().Type != TokenEnd {
		thenBody = append(thenBody, p.parseStmt())
	}
	var elseBody []Stmt
	if p.current().Type == TokenElse {
		p.consume(TokenElse)
		for p.current().Type != TokenEnd {
			elseBody = append(elseBody, p.parseStmt())
		}
	}
	p.consume(TokenEnd)
	return &IfStmt{Cond: cond, Then: thenBody, Else: elseBody}
}

func (p *Parser) parseReturn() *ReturnStmt {
	p.consume(TokenReturn)
	expr := p.parseExpr()
	return &ReturnStmt{Expr: expr}
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
		if tok.Type == TokenGT || tok.Type == TokenLT || tok.Type == TokenEQ {
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
	return p.parseRelational()
}

func (p *Parser) parseAdditive() Expr {
	expr := p.parseMultiplicative()
	for {
		tok := p.current()
		if tok.Type == TokenPlus || tok.Type == TokenMinus {
			p.pos++
			right := p.parseMultiplicative()
			expr = &BinaryExpr{Op: tok.Type, Left: expr, Right: right}
		} else {
			break
		}
	}
	return expr
}

func (p *Parser) parseMultiplicative() Expr {
	expr := p.parseUnary()
	for {
		tok := p.current()
		if tok.Type == TokenMul || tok.Type == TokenDiv {
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
	return p.parseDot()
}

func (p *Parser) parsePrimary() Expr {
	tok := p.current()
	var expr Expr
	switch tok.Type {
	case TokenMatch:
		p.pos++
		matchExpr := p.parseExpr()
		p.consume(TokenThen)
		var cases []MatchCaseExpr
		var defaultExpr Expr
		for p.current().Type != TokenEnd {
			if p.current().Type == TokenDefault {
				p.consume(TokenDefault)
				p.consume(TokenArrow)
				defaultExpr = p.parseExpr()
				break
			} else {
				var values []Expr
				values = append(values, p.parseExpr())
				for p.current().Type == TokenComma {
					p.pos++
					values = append(values, p.parseExpr())
				}
				p.consume(TokenArrow)
				body := p.parseExpr()
				cases = append(cases, MatchCaseExpr{Values: values, Body: body})
			}
		}
		p.consume(TokenEnd)
		expr = &MatchExpr{Expr: matchExpr, Cases: cases, Default: defaultExpr}
	case TokenNumber:
		p.pos++
		val, _ := strconv.ParseInt(tok.Value, 10, 64)
		expr = &NumberExpr{Value: val}
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
			// Accept both => and : for field assignment
			if p.current().Type == TokenFatArrow {
				p.consume(TokenFatArrow)
			} else {
				p.consume(TokenColon)
			}
			fieldExpr := p.parseExpr()
			fields[field] = fieldExpr
			if p.current().Type == TokenComma {
				p.pos++
			}
		}
		p.consume(TokenRBrace)
		expr = &RecordExpr{Fields: fields}
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
		} else if p.current().Type == TokenDot {
			p.pos++
			field := p.consume(TokenIdentifier).Value
			expr = &FieldAccessExpr{Receiver: expr, Field: field}
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
	if p.current().Type == TokenFunction {
		p.consume(TokenFunction)
		p.consume(TokenLParen)
		var params []Type
		if p.current().Type != TokenRParen {
			// Check if this is the new syntax with parameter names
			if p.peek().Type == TokenColon {
				// Parse parameter name and type: param_name: type
				p.consume(TokenIdentifier) // consume parameter name
				p.consume(TokenColon)
				params = append(params, p.parseType())
				for p.current().Type == TokenComma {
					p.pos++
					p.consume(TokenIdentifier) // consume parameter name
					p.consume(TokenColon)
					params = append(params, p.parseType())
				}
			} else {
				// Old syntax: just types
				params = append(params, p.parseType())
				for p.current().Type == TokenComma {
					p.pos++
					params = append(params, p.parseType())
				}
			}
		}
		p.consume(TokenRParen)
		p.consume(TokenColon)
		ret := p.parseType()
		return FunctionType{Params: params, Return: ret}
	} else {
		tok := p.consume(TokenIdentifier)
		switch tok.Value {
		case "int":
			return BasicType("int")
		case "string":
			return BasicType("string")
		case "bool":
			return BasicType("bool")
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
			expr = &FieldAccessExpr{Receiver: expr, Field: field}
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

func (p *Parser) parseRecordDecl() *AssignStmt {
	p.consume(TokenRecord)
	name := p.consume(TokenIdentifier).Value
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
	ty := RecordType{Fields: fields}
	expr := &TypeValueExpr{Ty: ty}
	return &AssignStmt{Var: name, DeclType: BasicType("type"), Expr: expr}
}

type TypeChecker struct {
	funcs map[string]struct {
		Params []Type
		Return Type
	}
	namedTypes map[string]Type
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{funcs: make(map[string]struct {
		Params []Type
		Return Type
	}), namedTypes: make(map[string]Type)}
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
			for _, p := range f.Params {
				params = append(params, p.Ty)
			}
			tc.funcs[f.Name] = struct {
				Params []Type
				Return Type
			}{Params: params, Return: f.ReturnType}
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
		// Check return type
		if len(s.Body) > 0 {
			if ret, ok := s.Body[len(s.Body)-1].(*ReturnStmt); ok {
				retTy, err := tc.typeCheckExpr(ret.Expr, localEnv)
				if err != nil {
					return err
				}
				retTy = tc.resolveType(retTy)
				if s.ReturnType != nil {
					if !tc.compatible(retTy, s.ReturnType) {
						return fmt.Errorf("return type mismatch: expected %v, got %v", s.ReturnType, retTy)
					}
					resolveTypes(ret.Expr, s.ReturnType)
					ret.Type = s.ReturnType
				} else {
					if isUnresolvedPlaceholder(retTy) {
						return fmt.Errorf("cannot infer return type for empty list without specified return type")
					}
					ret.Type = retTy
				}
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
			if !tc.compatible(ty, declType) {
				return fmt.Errorf("type mismatch: expected %v, got %v", declType, ty)
			}
			resolveTypes(s.Expr, declType)
			ty = declType
			if BasicType("type") == declType {
				if tve, ok := s.Expr.(*TypeValueExpr); ok {
					if rt, ok := tve.Ty.(RecordType); ok {
						tc.namedTypes[s.Var] = rt
					}
				}
			}
		} else if isUnresolvedPlaceholder(ty) {
			return fmt.Errorf("cannot infer type for empty list without declaration")
		}
		if existing, ok := env[s.Var]; ok {
			if !tc.equalTypes(existing, ty) {
				return fmt.Errorf("reassignment type mismatch: expected %v, got %v", existing, ty)
			}
			if s.DeclType != nil {
				return fmt.Errorf("cannot specify type on reassignment")
			}
		} else {
			env[s.Var] = ty
		}
		s.Type = ty
		return nil
	case *ReturnStmt:
		ty, err := tc.typeCheckExpr(s.Expr, env)
		if err != nil {
			return err
		}
		s.Type = ty
		return nil
	case *ExprStmt:
		ty, err := tc.typeCheckExpr(s.Expr, env)
		if err != nil {
			return err
		}
		s.Type = ty
		return nil
	case *IfStmt:
		_, err := tc.typeCheckExpr(s.Cond, env)
		if err != nil {
			return err
		}
		for _, st := range s.Then {
			if err := tc.typeCheckStmt(st, env); err != nil {
				return err
			}
		}
		for _, st := range s.Else {
			if err := tc.typeCheckStmt(st, env); err != nil {
				return err
			}
		}
		return nil
	case *WhileStmt:
		_, err := tc.typeCheckExpr(s.Cond, env)
		if err != nil {
			return err
		}
		for _, st := range s.Body {
			if err := tc.typeCheckStmt(st, env); err != nil {
				return err
			}
		}
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
				if !tc.equalTypes(valTy, matchTy) {
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
		if ty != BasicType("int") {
			return nil, fmt.Errorf("unary operator on non-int")
		}
		e.Type = BasicType("int")
		return BasicType("int"), nil
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
			} else if isComparisonOp(e.Op) {
				e.Type = BasicType("bool")
				return BasicType("bool"), nil
			} else if leftTy == BasicType("string") && e.Op == TokenPlus {
				e.Type = BasicType("string")
				return BasicType("string"), nil
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
				e.Type = BasicType("int") // placeholder
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
				return nil, fmt.Errorf("len called on non-list")
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
		if len(e.Args) != len(ft.Params) {
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
				instantiatedParam := tc.instantiateGenericType(ft.Params[i], typeMap)
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
		instantiatedReturn := tc.instantiateGenericType(ft.Return, typeMap)
		
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
				if !tc.equalTypes(valTy, matchTy) {
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
	case *TypeValueExpr:
		e.Type = BasicType("type")
		return BasicType("type"), nil
	default:
		return nil, fmt.Errorf("unsupported expression type")
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
	}
	return false
}

// Unify two types, handling generic type variables
func (tc *TypeChecker) unifyTypes(expected, actual Type, typeMap map[string]Type) (Type, error) {
	switch expectedType := expected.(type) {
	case GenericType:
		// If this generic type is already mapped, check consistency
		if mapped, exists := typeMap[expectedType.Name]; exists {
			if tc.equalTypes(mapped, actual) {
				return actual, nil
			}
			return nil, fmt.Errorf("type variable %s already unified to %v, cannot unify with %v", expectedType.Name, mapped, actual)
		}
		// Map the generic type to the actual type
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
	default:
		// For concrete types, they must match exactly
		if tc.equalTypes(expected, actual) {
			return actual, nil
		}
		return nil, fmt.Errorf("expected %v, got %v", expected, actual)
	}
}

// Instantiate a type by replacing generic type variables with concrete types
func (tc *TypeChecker) instantiateGenericType(t Type, typeMap map[string]Type) Type {
	switch typ := t.(type) {
	case GenericType:
		if mapped, exists := typeMap[typ.Name]; exists {
			return mapped
		}
		return t // Return unchanged if not mapped
	case ListType:
		return ListType{Element: tc.instantiateGenericType(typ.Element, typeMap)}
	case FunctionType:
		instantiatedParams := make([]Type, len(typ.Params))
		for i, param := range typ.Params {
			instantiatedParams[i] = tc.instantiateGenericType(param, typeMap)
		}
		return FunctionType{
			Params: instantiatedParams,
			Return: tc.instantiateGenericType(typ.Return, typeMap),
		}
	default:
		return t // Return concrete types unchanged
	}
}

func (tc *TypeChecker) compatible(a, b Type) bool {
	a = tc.resolveType(a)
	b = tc.resolveType(b)
	if a == nil {
		return true
	}
	if b == nil {
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
	default:
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
	case TokenPlus, TokenMinus, TokenMul, TokenDiv:
		return true
	default:
		return false
	}
}

func isComparisonOp(op TokenType) bool {
	switch op {
	case TokenGT, TokenLT, TokenEQ:
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
