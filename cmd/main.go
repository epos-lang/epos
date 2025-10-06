package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"lua_llvm/codegen"
	"lua_llvm/parser"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: lua_llvm <input.lua> [-o output]")
		os.Exit(1)
	}

	inputFile := os.Args[1]
	var outputFile string
	if len(os.Args) > 3 && os.Args[2] == "-o" {
		outputFile = os.Args[3]
	} else {
		base := filepath.Base(inputFile)
		name := strings.TrimSuffix(base, filepath.Ext(base))
		outputFile = "build/" + name
	}

	// Read Lua code
	code, err := os.ReadFile(inputFile)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		os.Exit(1)
	}

	// Parse and generate IR
	lexer := parser.NewLexer(string(code))
	tokens := lexer.Lex()
	p := parser.NewParser(tokens)
	stmts := p.Parse()

	cg := codegen.NewCodeGen()
	m := cg.Generate(stmts)

	irStr := m.String()
	fixed := strings.Replace(irStr, "declare i32 (i8*, ...) @printf()", "declare i32 @printf(i8*, ...)", 1)

	// Create build directory
	if err := os.MkdirAll("build", 0755); err != nil {
		fmt.Printf("Error creating build directory: %v\n", err)
		os.Exit(1)
	}

	// Write IR to temp file
	tempLL := "build/temp.ll"
	if err := os.WriteFile(tempLL, []byte(fixed), 0644); err != nil {
		fmt.Printf("Error writing IR file: %v\n", err)
		os.Exit(1)
	}

	// Compile to assembly
	tempS := "build/temp.s"
	cmd := exec.Command("llc", tempLL, "-o", tempS)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		fmt.Printf("Error running llc: %v\n", err)
		fmt.Println("Stdout:", stdout.String())
		fmt.Println("Stderr:", stderr.String())
		os.Exit(1)
	}

	// Compile to executable
	cmd = exec.Command("clang", tempS, "-o", outputFile, "-e", "lua_main")
	var clangStdout, clangStderr bytes.Buffer
	cmd.Stdout = &clangStdout
	cmd.Stderr = &clangStderr
	if err := cmd.Run(); err != nil {
		fmt.Printf("Error running clang: %v\n", err)
		fmt.Println("Stdout:", clangStdout.String())
		fmt.Println("Stderr:", clangStderr.String())
		os.Exit(1)
	}

	// Clean up temp files
	os.Remove(tempLL)
	os.Remove(tempS)

	fmt.Printf("Executable created: %s\n", outputFile)
}
