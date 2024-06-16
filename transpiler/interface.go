package transpiler

import (
	"go/ast"
	"go/token"
)

func TranspileFile(fset *token.FileSet, filename string, src any, trace bool) (*ast.File, error) {
	if fset == nil {
		panic("transpiler.TranspileFile: no token.Fileset porvided (fset == nil)")
	}

	text, err := readSource(filename, src)
	if err != nil {
		return nil, err
	}

	var transpiler Transpiler
	transpiler.init(fset, filename, text, trace)

	return transpiler.transpile(), nil
}
