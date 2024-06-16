package transformer

import (
	"go/ast"
	"goX/transpiler/astx"
)

func (t *Transformer) transformImportSpec(x *astx.ImportSpec) *ast.ImportSpec {
	if t.trace {
		defer un(trace(t, "TransformImportSpec"))
	}
	if x == nil {
		return nil
	}

	return &ast.ImportSpec{
		Doc:     t.transformCommentGroup(),
		Name:    t.transformIdent(x.Name),
		Path:    t.transformBasicLit(x.Path),
		Comment: t.transformCommentGroup(),
		EndPos:  x.EndPos,
	}
}

func (t *Transformer) transformValueSpec(x *astx.ValueSpec) *ast.ValueSpec {
	if t.trace {
		defer un(trace(t, "transformValueSpec"))
	}
	if x == nil {
		return nil
	}

	var names []*ast.Ident
	for _, name := range x.Names {
		names = append(names, t.transformIdent(name))
	}
	ty := t.transformExpr(x.Type)
	var values []ast.Expr
	for _, val := range x.Values {
		values = append(values, t.transformExpr(val))
	}

	comment := t.transformCommentGroup()
	return &ast.ValueSpec{
		Doc:     t.transformCommentGroup(),
		Names:   names,
		Type:    ty,
		Values:  values,
		Comment: comment,
	}
}

func (t *Transformer) transformTypeSpec(x *astx.TypeSpec) *ast.TypeSpec {
	if t.trace {
		defer un(trace(t, "transformTypeSpec"))
	}
	if x == nil {
		return nil
	}

	return &ast.TypeSpec{
		Doc:        t.transformCommentGroup(),
		Name:       t.transformIdent(x.Name),
		TypeParams: t.transformFieldList(x.TypeParams),
		Assign:     x.Assign,
		Type:       t.transformExpr(x.Type),
		Comment:    t.transformCommentGroup(),
	}
}

func (t *Transformer) transformSpec(x astx.Spec) ast.Spec {
	if t.trace {
		defer un(trace(t, "transformSpec"))
	}
	if x == nil {
		return nil
	}
	switch spec := x.(type) {
	case *astx.ImportSpec:
		return t.transformImportSpec(spec)
	case *astx.ValueSpec:
		return t.transformValueSpec(spec)
	case *astx.TypeSpec:
		return t.transformTypeSpec(spec)
	default:
		return nil
	}
}
