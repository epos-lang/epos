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
	code := `i = 0
while i < 5 do
print(i)
i = i + 1
end`

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
