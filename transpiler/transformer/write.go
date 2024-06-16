package transformer

import (
	"fmt"
	"go/ast"
	"go/token"
	"goX/transpiler/astx"
)

func (t *Transformer) writeIdent(name string) *ast.Ident {
	return &ast.Ident{NamePos: token.NoPos, Name: name, Obj: nil}
}

func (t *Transformer) writeBlockStmt(a ast.Stmt, b *ast.BlockStmt, c ast.Stmt) *ast.BlockStmt {
	list := []ast.Stmt{a}
	if b != nil {
		list = append(list, b.List...)
	}
	list = append(list, c)
	return &ast.BlockStmt{List: list}
}

func (t *Transformer) writeCall(x, fun *ast.Ident, args ...ast.Expr) *ast.ExprStmt {
	return &ast.ExprStmt{
		X: &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   x,
				Sel: fun,
			},
			Args: args,
		},
	}
}

func (t *Transformer) writeAssign(v, x, fun *ast.Ident, args ...ast.Expr) *ast.AssignStmt {
	return &ast.AssignStmt{
		Lhs: []ast.Expr{v},
		Tok: token.DEFINE,
		Rhs: []ast.Expr{
			&ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   x,
					Sel: fun,
				},
				Args: args,
			},
		},
	}
}

func (t *Transformer) writeNewElement(tag *ast.BasicLit, sc bool) (*ast.AssignStmt, *ast.Ident) {
	ni := t.writeIdent(t.nextName())
	pi := t.writeIdent("gox")
	var fi *ast.Ident
	if sc {
		fi = t.writeIdent("E1")
	} else {
		fi = t.writeIdent("E")
	}
	tag.Value = fmt.Sprintf("\"%s\"", tag.Value)
	return t.writeAssign(ni, pi, fi, tag), ni
}

func (t *Transformer) writeAddText(parent *ast.Ident, args ...ast.Expr) *ast.ExprStmt {
	at := t.writeIdent("T")
	return t.writeCall(parent, at, args...)
}

func (t *Transformer) writeAddChild(parent, child *ast.Ident) *ast.ExprStmt {
	ac := t.writeIdent("C")
	return t.writeCall(parent, ac, child)
}

func (t *Transformer) writeAddCallChild(parent *ast.Ident, call ast.Expr) *ast.ExprStmt {
	ac := t.writeIdent("C1")
	return t.writeCall(parent, ac, call)
}

func (t *Transformer) openingTagName(x *astx.HtmlElem) (*ast.BasicLit, bool) {
	bl := &ast.BasicLit{ValuePos: x.Pos(), Kind: token.STRING, Value: x.Opening.Name.Name}
	return bl, x.SelfClosing
}

func (t *Transformer) writeGoxComponent(name *ast.Ident, declaration *ast.AssignStmt, body *ast.BlockStmt) *ast.FuncLit {
	ret := &ast.ReturnStmt{
		Results: []ast.Expr{name},
	}
	block := t.writeBlockStmt(declaration, body, ret)

	return &ast.FuncLit{
		Type: &ast.FuncType{
			Results: &ast.FieldList{
				List: []*ast.Field{
					{Type: &ast.SelectorExpr{
						X:   t.writeIdent("gox"),
						Sel: t.writeIdent("Element"),
					}},
				},
			},
		},
		Body: block,
	}
}
