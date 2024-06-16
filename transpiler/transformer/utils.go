package transformer

import (
	"go/ast"
	"go/token"
	"strconv"
)

func (t *Transformer) nextName() string {
	name := t.name + strconv.Itoa(t.nameIndex)
	t.nameIndex++
	return name
}

func (t *Transformer) resetName() {
	t.nameIndex = 0
}

func (t *Transformer) importGoX(file *ast.File) {
	importPath := "\"goX/gox\""
	goxImport := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: importPath,
		},
	}

	for _, decl := range file.Decls {
		if genDecl, ok := decl.(*ast.GenDecl); ok && genDecl.Tok == token.IMPORT {
			for _, spec := range genDecl.Specs {
				if importSpec, ok := spec.(*ast.ImportSpec); ok && importSpec.Path.Value == importPath {
					return
				}
			}
			genDecl.Specs = append(genDecl.Specs, goxImport)
			file.Imports = append(file.Imports, goxImport)
			return
		}
	}

	newImportDecl := &ast.GenDecl{
		Tok:   token.IMPORT,
		Specs: []ast.Spec{goxImport},
	}
	file.Decls = append([]ast.Decl{newImportDecl}, file.Decls...)
	file.Imports = append(file.Imports, goxImport)
}
