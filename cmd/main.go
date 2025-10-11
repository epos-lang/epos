package main

import (
	"bytes"
	"epos/codegen"
	"epos/parser"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func parseWithImports(filePath string, visited map[string]bool) ([]parser.Stmt, error) {
	// Check for circular imports
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve path %s: %v", filePath, err)
	}
	
	if visited[absPath] {
		return nil, fmt.Errorf("circular import detected: %s", absPath)
	}
	visited[absPath] = true
	
	// Read and parse the file
	code, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	
	lexer := parser.NewLexer(string(code))
	tokens := lexer.Lex()
	p := parser.NewParser(tokens)
	stmts := p.Parse()
	
	// Collect all statements, processing imports
	var allStmts []parser.Stmt
	baseDir := filepath.Dir(absPath)
	
	for _, stmt := range stmts {
		if importStmt, ok := stmt.(*parser.ImportStmt); ok {
			// Resolve import path relative to current file
			var importPath string
			if filepath.IsAbs(importStmt.Path) {
				importPath = importStmt.Path
			} else {
				importPath = filepath.Join(baseDir, importStmt.Path)
			}
			
			// Recursively parse imported file
			importedStmts, err := parseWithImports(importPath, visited)
			if err != nil {
				return nil, fmt.Errorf("error parsing import %s: %v", importPath, err)
			}
			
			// Add imported statements (only functions needed for compilation)
			for _, importedStmt := range importedStmts {
				if funcStmt, ok := importedStmt.(*parser.FunctionStmt); ok {
					// Only include functions that are actually imported
					if importStmt.IsWildcard && funcStmt.IsPublic {
						allStmts = append(allStmts, importedStmt)
					} else if len(importStmt.Items) > 0 {
						for _, item := range importStmt.Items {
							if funcStmt.Name == item {
								allStmts = append(allStmts, importedStmt)
								break
							}
						}
					} else if importStmt.Alias != "" && funcStmt.Name == importStmt.Alias {
						allStmts = append(allStmts, importedStmt)
					}
				}
			}
		} else {
			// Regular statement
			allStmts = append(allStmts, stmt)
		}
	}
	
	return allStmts, nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: epos <input.epos> [-o output] [-r]")
		os.Exit(1)
	}

	var inputFile, outputFile string
	var runAfterCompile bool

	// Parse arguments
	args := os.Args[1:]
	for i, arg := range args {
		if arg == "-r" {
			runAfterCompile = true
		} else if arg == "-o" && i+1 < len(args) {
			outputFile = args[i+1]
			i++ // Skip next arg since it's the output filename
		} else if !strings.HasPrefix(arg, "-") && inputFile == "" {
			inputFile = arg
		}
	}

	if inputFile == "" {
		fmt.Println("Error: No input file specified")
		fmt.Println("Usage: epos <input.epos> [-o output] [-r]")
		os.Exit(1)
	}

	if outputFile == "" {
		base := filepath.Base(inputFile)
		name := strings.TrimSuffix(base, filepath.Ext(base))
		outputFile = "build/" + name
	}

	// Parse and generate IR with import resolution
	allStmts, err := parseWithImports(inputFile, make(map[string]bool))
	if err != nil {
		fmt.Printf("Error parsing with imports: %v\n", err)
		os.Exit(1)
	}

	tc := parser.NewTypeChecker()
	if err := tc.TypeCheck(allStmts); err != nil {
		fmt.Printf("Type error: %v\n", err)
		os.Exit(1)
	}

	cg := codegen.NewCodeGen()
	m := cg.Generate(allStmts)

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
	cmd = exec.Command("clang", tempS, "-o", outputFile)
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

	// Run the executable if -r flag was provided
	if runAfterCompile {
		fmt.Printf("Running %s...\n", outputFile)
		cmd = exec.Command("./" + outputFile)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Printf("Error running executable: %v\n", err)
			os.Exit(1)
		}
	}
}
