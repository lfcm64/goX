package transformer

import (
	"go/ast"
	"goX/transpiler/astx"
)

func (t *Transformer) transformBadDecl(x *astx.BadDecl) *ast.BadDecl {
	if t.trace {
		defer un(trace(t, "transformBadDecl"))
	}
	if x == nil {
		return nil
	}
	return &ast.BadDecl{
		From: x.From,
		To:   x.To,
	}
}

func (t *Transformer) transformGenDecl(x *astx.GenDecl) *ast.GenDecl {
	if t.trace {
		defer un(trace(t, "transformGenDecl"))
	}
	if x == nil {
		return nil
	}

	var specs []ast.Spec
	for _, spec := range x.Specs {
		specs = append(specs, t.transformSpec(spec))
	}

	return &ast.GenDecl{
		Doc:    t.transformCommentGroup(),
		TokPos: x.TokPos,
		Tok:    x.Tok,
		Lparen: x.Lparen,
		Specs:  specs,
		Rparen: x.Rparen,
	}
}

func (t *Transformer) transformFuncDecl(x *astx.FuncDecl) *ast.FuncDecl {
	if t.trace {
		defer un(trace(t, "transformFuncDecl"))
	}
	if x == nil {
		return nil
	}

	return &ast.FuncDecl{
		Doc:  t.transformCommentGroup(),
		Recv: t.transformFieldList(x.Recv),
		Name: t.transformIdent(x.Name),
		Type: t.transformFuncType(x.Type),
		Body: t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformDecl(x astx.Decl) ast.Decl {
	if t.trace {
		defer un(trace(t, "TransformDecl"))
	}
	if x == nil {
		return nil
	}
	switch decl := x.(type) {
	case *astx.BadDecl:
		return t.transformBadDecl(decl)
	case *astx.GenDecl:
		return t.transformGenDecl(decl)
	case *astx.FuncDecl:
		return t.transformFuncDecl(decl)
	default:
		return nil
	}
}
