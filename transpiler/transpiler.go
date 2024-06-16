package transpiler

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"goX/transpiler/astx"
	"goX/transpiler/parser"
	"goX/transpiler/transformer"
)

type Transpiler struct {
	filename string
	fset     *token.FileSet
	src      []byte
	tfile    *token.File

	errors ErrorList

	fileA *astx.File
	fileB *ast.File

	trace bool
}

func (t *Transpiler) init(fset *token.FileSet, filename string, src []byte, trace bool) {
	t.filename = filename
	t.tfile = fset.AddFile(filename, -1, len(src))
	t.fset = fset
	t.src = src
	t.trace = trace
}

func (t *Transpiler) transpile() *ast.File {
	if t.trace {
		fmt.Print("--Parser--\n\n")
	}
	t.parse()
	t.analyse()
	if t.trace {
		fmt.Print("\n\n--Transformer--\n\n")
	}
	t.transform()
	t.check()
	return t.fileB
}

func (t *Transpiler) parse() {
	var parser parser.Parser
	parser.Init(t.tfile, t.src, t.report, t.trace)

	t.fileA = parser.ParseFile()
}

func (t *Transpiler) analyse() {
	var a analyser
	a.init(t.tfile, t.fileA, t.report)

	a.analyseFile()
}

func (t *Transpiler) transform() {
	var transformer transformer.Transformer
	transformer.Init(t.fileA, t.trace)

	t.fileB = transformer.TransformFile()
}

func (t *Transpiler) check() {
	conf := types.Config{
		FakeImportC: true,
		Error: func(err error) {
			t.report0(err)
		},
		//Importer: importer.ForCompiler(t.fset, *compiler, nil),
		//Sizes:    types.SizesFor(build.Default.Compiler, build.Default.GOARCH),
	}

	files := []*ast.File{t.fileB}
	conf.Check(t.filename, t.fset, files, nil)
}

type ErrorList []error

func (e *ErrorList) Add(err error) {
	*e = append(*e, err)
}

func (e ErrorList) Lenght() int {
	return len(e)
}
