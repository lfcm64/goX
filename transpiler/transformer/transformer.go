package transformer

import (
	"fmt"
	"go/ast"
	"goX/transpiler/astx"
)

type Transformer struct {
	file *astx.File

	elem bool

	nameIndex int
	name      string

	indent int
	trace  bool
}

func (t *Transformer) Init(file *astx.File, trace bool) {
	var n namer
	t.name = n.findName(file)

	t.file = file
	t.trace = trace
}

func (t *Transformer) printTrace(a ...any) {
	const dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
	const n = len(dots)
	i := 2*t.indent + 6
	for i > n {
		fmt.Print(dots)
		i -= n
	}
	// i <= n
	fmt.Print("    ")
	fmt.Print(dots[0:i])
	fmt.Println(a...)
}

func trace(t *Transformer, msg string) *Transformer {
	t.printTrace(msg, "(")
	t.indent++
	return t
}

func un(t *Transformer) {
	t.indent--
	t.printTrace(")")
}

func (t *Transformer) TransformFile() *ast.File {
	if t.trace {
		t.printTrace("TransformFile")
	}

	imports := make([]*ast.ImportSpec, len(t.file.Imports))
	for i, imp := range t.file.Imports {
		imports[i] = t.transformImportSpec(imp)
	}

	decls := make([]ast.Decl, len(t.file.Decls))
	for i, decl := range t.file.Decls {
		decls[i] = t.transformDecl(decl)
	}

	unresolved := make([]*ast.Ident, len(t.file.Unresolved))
	for i, u := range t.file.Unresolved {
		unresolved[i] = t.transformIdent(u)
	}

	file := &ast.File{
		Package:    t.file.Package,
		Name:       t.transformIdent(t.file.Name),
		Decls:      decls,
		FileStart:  t.file.FileStart,
		FileEnd:    t.file.FileEnd,
		Scope:      t.file.Scope,
		Imports:    imports,
		Unresolved: unresolved,
		GoVersion:  t.file.GoVersion,
	}

	if t.elem {
		t.importGoX(file)
	}

	return file
}

func (t *Transformer) transformCommentGroup() *ast.CommentGroup { return &ast.CommentGroup{} }

func (t *Transformer) transformField(x *astx.Field) *ast.Field {
	if t.trace {
		defer un(trace(t, "transformField"))
	}
	if x == nil {
		return nil
	}

	var names []*ast.Ident
	for _, name := range x.Names {
		names = append(names, t.transformIdent(name))
	}

	return &ast.Field{
		Doc:     t.transformCommentGroup(),
		Names:   names,
		Type:    t.transformExpr(x.Type),
		Tag:     t.transformBasicLit(x.Tag),
		Comment: t.transformCommentGroup(),
	}
}

func (t *Transformer) transformFieldList(x *astx.FieldList) *ast.FieldList {
	if x == nil {
		return nil
	}
	if t.trace && len(x.List) != 0 {
		defer un(trace(t, "transformFieldList"))
	}

	var list []*ast.Field
	for _, field := range x.List {
		list = append(list, t.transformField(field))
	}

	return &ast.FieldList{
		Opening: x.Opening,
		List:    list,
		Closing: x.Closing,
	}
}
