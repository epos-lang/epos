package parser

import (
	"fmt"
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
	TokenMul
	TokenDiv
	TokenAssign
	TokenPrint
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
)

// Token struct
type Token struct {
	Type  TokenType
	Value string
}

// AST nodes
type Expr interface{}

type NumberExpr struct {
	Value float64
}

type VarExpr struct {
	Name string
}

type BinaryExpr struct {
	Op    TokenType
	Left  Expr
	Right Expr
}

type UnaryExpr struct {
	Op   TokenType
	Expr Expr
}

type Stmt interface{}

type AssignStmt struct {
	Var  string
	Expr Expr
}

type PrintStmt struct {
	Expr Expr
}

type FunctionStmt struct {
	Name   string
	Params []string
	Body   []Stmt
}

type CallExpr struct {
	Callee string
	Args   []Expr
}

type ReturnStmt struct {
	Expr Expr
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

type StringExpr struct {
	Value string
}

type ExprStmt struct {
	Expr Expr
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
			l.lexString()
		case ch == '+':
			l.addToken(TokenPlus, "+")
		case ch == '-':
			if l.peekChar() == '-' {
				l.lexComment()
			} else {
				l.addToken(TokenMinus, "-")
			}
		case ch == '*':
			l.addToken(TokenMul, "*")
		case ch == '/':
			l.addToken(TokenDiv, "/")
		case ch == '=':
			if l.peekChar() == '=' {
				l.pos++
				l.addToken(TokenEQ, "==")
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
		case ch == '(':
			l.addToken(TokenLParen, "(")
		case ch == ')':
			l.addToken(TokenRParen, ")")
		default:
			panic(fmt.Sprintf("unexpected character: %c", ch))
		}
	}
	l.addToken(TokenEOF, "")
	return l.tokens
}

func (l *Lexer) lexComment() {
	l.pos += 2 // skip --
	if l.input[l.pos] == '[' && l.peekChar() == '[' {
		l.pos += 2 // skip [[
		for l.pos < len(l.input) {
			if l.pos+1 < len(l.input) && l.input[l.pos] == ']' && l.input[l.pos+1] == ']' {
				l.pos += 2
				return
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

func (l *Lexer) lexString() {
	l.pos++ // skip opening "
	start := l.pos
	for l.pos < len(l.input) && l.input[l.pos] != '"' {
		l.pos++
	}
	str := l.input[start:l.pos]
	l.pos++ // skip closing "
	l.tokens = append(l.tokens, Token{Type: TokenString, Value: str})
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
	for l.pos < len(l.input) && (unicode.IsLetter(rune(l.input[l.pos])) || unicode.IsDigit(rune(l.input[l.pos]))) {
		l.pos++
	}
	id := l.input[start:l.pos]
	if id == "print" {
		l.tokens = append(l.tokens, Token{Type: TokenPrint, Value: id})
	} else if id == "fn" {
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
	if tok.Type == TokenIdentifier && p.peek().Type == TokenAssign {
		return p.parseAssign()
	} else if tok.Type == TokenPrint {
		return p.parsePrint()
	} else if tok.Type == TokenFunction {
		return p.parseFunction()
	} else if tok.Type == TokenReturn {
		return p.parseReturn()
	} else if tok.Type == TokenIf {
		return p.parseIf()
	} else if tok.Type == TokenWhile {
		return p.parseWhile()
	} else {
		return &ExprStmt{Expr: p.parseExpr()}
	}
}

func (p *Parser) parseAssign() *AssignStmt {
	varName := p.consume(TokenIdentifier).Value
	p.consume(TokenAssign)
	expr := p.parseExpr()
	return &AssignStmt{Var: varName, Expr: expr}
}

func (p *Parser) parsePrint() *PrintStmt {
	p.consume(TokenPrint)
	expr := p.parseExpr()
	return &PrintStmt{Expr: expr}
}

func (p *Parser) parseFunction() *FunctionStmt {
	p.consume(TokenFunction)
	name := p.consume(TokenIdentifier).Value
	p.consume(TokenLParen)
	var params []string
	if p.current().Type != TokenRParen {
		params = append(params, p.consume(TokenIdentifier).Value)
		for p.current().Type == TokenComma {
			p.pos++
			params = append(params, p.consume(TokenIdentifier).Value)
		}
	}
	p.consume(TokenRParen)
	var body []Stmt
	for p.current().Type != TokenEnd {
		body = append(body, p.parseStmt())
	}
	p.consume(TokenEnd)
	return &FunctionStmt{Name: name, Params: params, Body: body}
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
	return p.parsePrimary()
}

func (p *Parser) parsePrimary() Expr {
	tok := p.current()
	switch tok.Type {
	case TokenNumber:
		p.pos++
		val, _ := strconv.ParseFloat(tok.Value, 64)
		return &NumberExpr{Value: val}
	case TokenString:
		p.pos++
		return &StringExpr{Value: tok.Value}
	case TokenIdentifier:
		name := tok.Value
		p.pos++
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
			return &CallExpr{Callee: name, Args: args}
		}
		return &VarExpr{Name: name}
	case TokenLParen:
		p.pos++
		expr := p.parseExpr()
		p.consume(TokenRParen)
		return expr
	default:
		panic(fmt.Sprintf("unexpected token in primary: %v", tok))
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

func (p *Parser) consume(tt TokenType) Token {
	tok := p.current()
	if tok.Type != tt {
		panic(fmt.Sprintf("expected %d, got %d", tt, tok.Type))
	}
	p.pos++
	return tok
}
