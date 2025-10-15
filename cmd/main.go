package main

import (
	"archive/zip"
	"bytes"
	"epos/codegen"
	"epos/parser"
	"fmt"
	"io"
	"net/http"
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

func initProject(projectName string) error {
	// Download repository as zip
	resp, err := http.Get("https://github.com/epos-lang/simple-program-in-epos/archive/master.zip")
	if err != nil {
		return fmt.Errorf("failed to download template: %v", err)
	}
	defer resp.Body.Close()

	// Create temporary file for zip
	tmpFile, err := os.CreateTemp("", "epos-init-*.zip")
	if err != nil {
		return fmt.Errorf("failed to create temp file: %v", err)
	}
	defer os.Remove(tmpFile.Name())
	defer tmpFile.Close()

	// Download zip content
	_, err = io.Copy(tmpFile, resp.Body)
	if err != nil {
		return fmt.Errorf("failed to download zip: %v", err)
	}

	// Extract zip
	zipReader, err := zip.OpenReader(tmpFile.Name())
	if err != nil {
		return fmt.Errorf("failed to open zip: %v", err)
	}
	defer zipReader.Close()

	// Create project directory
	if err := os.MkdirAll(projectName, 0755); err != nil {
		return fmt.Errorf("failed to create project directory: %v", err)
	}

	// Extract files
	for _, file := range zipReader.File {
		// Skip the root directory (simple-program-in-epos-master/)
		parts := strings.Split(file.Name, "/")
		if len(parts) <= 1 {
			continue
		}
		relativePath := strings.Join(parts[1:], "/")
		if relativePath == "" {
			continue
		}

		destPath := filepath.Join(projectName, relativePath)

		if file.FileInfo().IsDir() {
			os.MkdirAll(destPath, file.FileInfo().Mode())
			continue
		}

		// Create parent directories
		if err := os.MkdirAll(filepath.Dir(destPath), 0755); err != nil {
			return fmt.Errorf("failed to create directory %s: %v", filepath.Dir(destPath), err)
		}

		// Extract file
		reader, err := file.Open()
		if err != nil {
			return fmt.Errorf("failed to open file in zip: %v", err)
		}

		outFile, err := os.Create(destPath)
		if err != nil {
			reader.Close()
			return fmt.Errorf("failed to create file %s: %v", destPath, err)
		}

		_, err = io.Copy(outFile, reader)
		reader.Close()
		outFile.Close()

		if err != nil {
			return fmt.Errorf("failed to write file %s: %v", destPath, err)
		}
	}

	// Replace names in files
	filesToUpdate := []string{
		filepath.Join(projectName, "README.md"),
		filepath.Join(projectName, "flake.nix"),
	}

	for _, filePath := range filesToUpdate {
		content, err := os.ReadFile(filePath)
		if err != nil {
			continue // Skip if file doesn't exist
		}

		// Replace "simple-program-in-epos" with project name
		updatedContent := strings.ReplaceAll(string(content), "simple-program-in-epos", projectName)
		updatedContent = strings.ReplaceAll(updatedContent, "Simple program in Epos", "Simple program in Epos: "+projectName)
		updatedContent = strings.ReplaceAll(updatedContent, "simple-epos-app", projectName+"-app")

		if err := os.WriteFile(filePath, []byte(updatedContent), 0644); err != nil {
			return fmt.Errorf("failed to update file %s: %v", filePath, err)
		}
	}

	fmt.Printf("Project %s created successfully!\n", projectName)
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage:")
		fmt.Println("  epos init <project-name>            Create a new Epos project")
		fmt.Println("  epos <input.epos> [-o output] [-r] [program_args...]   Compile an Epos file")
		os.Exit(1)
	}

	// Handle init command
	if os.Args[1] == "init" {
		if len(os.Args) < 3 {
			fmt.Println("Usage: epos init <project-name>")
			os.Exit(1)
		}
		projectName := os.Args[2]
		if err := initProject(projectName); err != nil {
			fmt.Printf("Error initializing project: %v\n", err)
			os.Exit(1)
		}
		return
	}

	var inputFile, outputFile string
	var runAfterCompile bool
	var programArgs []string

	// Parse arguments
	args := os.Args[1:]
	skipNext := false
	for i, arg := range args {
		if skipNext {
			skipNext = false
			continue
		}
		if arg == "-r" {
			runAfterCompile = true
			// All remaining args after -r are program arguments
			if i+1 < len(args) {
				// Find the input file first, then collect remaining args as program args
				remainingArgs := args[i+1:]
				foundInput := false
				for j, remainingArg := range remainingArgs {
					if !strings.HasPrefix(remainingArg, "-") && !foundInput {
						if inputFile == "" {
							inputFile = remainingArg
							foundInput = true
						}
					} else if foundInput {
						// Everything after the input file are program arguments
						programArgs = remainingArgs[j:]
						break
					}
				}
			}
		} else if arg == "-o" && i+1 < len(args) {
			outputFile = args[i+1]
			skipNext = true
		} else if !strings.HasPrefix(arg, "-") && inputFile == "" {
			inputFile = arg
		}
	}

	if inputFile == "" {
		fmt.Println("Error: No input file specified")
		fmt.Println("Usage: epos <input.epos> [-o output] [-r] [program_args...]")
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

	cg := codegen.NewCodeGen(tc.GetNamedTypes())
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

	// Keep temp.ll for debugging, only clean up .s file
	os.Remove(tempS)

	fmt.Printf("Executable created: %s\n", outputFile)

	// Run the executable if -r flag was provided
	if runAfterCompile {
		fmt.Printf("Running %s", outputFile)
		if len(programArgs) > 0 {
			fmt.Printf(" with args: %v", programArgs)
		}
		fmt.Println("...")
		
		// Build command with program arguments
		cmdArgs := append([]string{"./" + outputFile}, programArgs...)
		cmd = exec.Command(cmdArgs[0], cmdArgs[1:]...)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Printf("Error running executable: %v\n", err)
			os.Exit(1)
		}
	}
}
