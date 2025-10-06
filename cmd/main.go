package main

import (
	"fmt"
	"lua_llvm/codegen"
	"lua_llvm/parser"
	"os"
	"strings"
	//"os/exec"
)

func main() {
	code := `function add(x, y)
return x + y
end

print(add(1, 2))`

	lexer := parser.NewLexer(code)
	tokens := lexer.Lex()
	p := parser.NewParser(tokens)
	stmts := p.Parse()

	cg := codegen.NewCodeGen()
	m := cg.Generate(stmts)

	irStr := m.String()
	fixed := strings.Replace(irStr, "declare i32 (i8*, ...) @printf()", "declare i32 @printf(i8*, ...)", 1)
	fmt.Println(fixed)

	err := os.WriteFile("out.ll", []byte(fixed), 0644)
	if err != nil {
		fmt.Println(err)
	}

	// exec.Command("llc", "out.ll", "-o", "out.s")
	// exec.Command("clang", "out.s", "-o", "out")
}
