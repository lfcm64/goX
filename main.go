package main

import (
	"fmt"
	"go/printer"
	"go/token"
	"goX/transpiler"
	"os"
)

func main() {
	// Create a new FileSet
	fset := token.NewFileSet()

	// Transpile the file
	file, err := transpiler.TranspileFile(fset, "test.txt", nil, false)
	if err != nil {
		fmt.Println("Error transpiling file:", err)
		return
	}

	//ast.Print(fset, file)

	// Open a file for writing
	outputFile, err := os.Create("ast_output.go")
	if err != nil {
		fmt.Println("Error creating file:", err)
		return
	}
	defer outputFile.Close()

	// Print the AST to the file using go/printer
	err = printer.Fprint(outputFile, fset, file)
	if err != nil {
		fmt.Println("Error printing AST to file:", err)
		return
	}
}
