package transformer

import (
	"go/ast"
	"goX/transpiler/astx"
)

func (t *Transformer) transformTextElem(parent *ast.Ident, x *astx.TextElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformTextElem"))
	}
	list := make([]ast.Expr, len(x.List))
	for i, expr := range x.List {
		list[i] = t.transformExpr(expr)
	}
	at := t.writeAddText(parent, list...)

	return &ast.BlockStmt{List: []ast.Stmt{at}}
}

func (t *Transformer) transformForElem(parent *ast.Ident, x *astx.ForElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformForElem"))
	}

	fs := &ast.ForStmt{
		For:  x.For,
		Init: t.transformStmt(x.Init),
		Cond: t.transformExpr(x.Cond),
		Post: t.transformStmt(x.Post),
		Body: t.tranformBlockElem(parent, x.Body),
	}
	return &ast.BlockStmt{List: []ast.Stmt{fs}}
}

func (t *Transformer) transformRangeElem(parent *ast.Ident, x *astx.RangeElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformRangeElem"))
	}

	rs := &ast.RangeStmt{
		For:    x.For,
		Key:    t.transformExpr(x.Key),
		Value:  t.transformExpr(x.Value),
		TokPos: x.TokPos,
		Tok:    x.Tok,
		Range:  x.Range,
		X:      t.transformExpr(x.X),
		Body:   t.tranformBlockElem(parent, x.Body),
	}
	return &ast.BlockStmt{List: []ast.Stmt{rs}}
}

func (t *Transformer) transformIfElem(parent *ast.Ident, x *astx.IfElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformIfElem"))
	}

	is := &ast.IfStmt{
		Init: t.transformStmt(x.Init),
		Cond: t.transformExpr(x.Cond),
		Body: t.tranformBlockElem(parent, x.Body),
	}

	els := t.transformElem(parent, x.Else)
	if els != nil {
		is.Else = els
	}

	return &ast.BlockStmt{List: []ast.Stmt{is}}
}

func (t *Transformer) transformElemCaseClause(parent *ast.Ident, x *astx.ElemCaseClause) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformElemCaseClause"))
	}

	var list []ast.Expr
	for _, item := range x.List {
		list = append(list, t.transformExpr(item))
	}
	body := t.tranformBlockElem(parent, x.Body)

	cs := &ast.CaseClause{
		Case:  x.Case,
		List:  list,
		Colon: x.Colon,
		Body:  body.List,
	}
	return &ast.BlockStmt{List: []ast.Stmt{cs}}
}

func (t *Transformer) transformSwitchElem(parent *ast.Ident, x *astx.SwitchElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformSwitchElem"))
	}
	ss := &ast.SwitchStmt{
		Switch: x.Switch,
		Init:   t.transformStmt(x.Init),
		Tag:    t.transformExpr(x.Tag),
		Body:   t.tranformBlockElem(parent, x.Body),
	}
	return &ast.BlockStmt{List: []ast.Stmt{ss}}
}

func (t *Transformer) transformTypeSwitchElem(parent *ast.Ident, x *astx.TypeSwitchElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformTypeSwitchElem"))
	}
	ss := &ast.TypeSwitchStmt{
		Switch: x.Switch,
		Init:   t.transformStmt(x.Init),
		Assign: t.transformStmt(x.Assign),
		Body:   t.tranformBlockElem(parent, x.Body),
	}
	return &ast.BlockStmt{List: []ast.Stmt{ss}}
}

func (t *Transformer) transformElemCall(parent *ast.Ident, x *astx.ElemCall) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformElemCall"))
	}
	call := t.transformCallExpr(x.Call)
	add := t.writeAddCallChild(parent, call)
	return &ast.BlockStmt{List: []ast.Stmt{add}}
}

func (t *Transformer) transformHtmlElem(parent *ast.Ident, x *astx.HtmlElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformHtmlElem"))
	}
	tag, sc := t.openingTagName(x)

	decl, name := t.writeNewElement(tag, sc)
	body := t.tranformBlockElem(name, x.Body)
	add := t.writeAddChild(parent, name)

	return t.writeBlockStmt(decl, body, add)
}

func (t *Transformer) transformHtmlExpr(x *astx.HtmlElem) ast.Expr {
	if t.trace {
		defer un(trace(t, "transformHtmlExpr"))
	}
	tag, sc := t.openingTagName(x)

	decl, name := t.writeNewElement(tag, sc)
	body := t.tranformBlockElem(name, x.Body)

	t.resetName()
	t.elem = true
	return t.writeGoxComponent(name, decl, body)
}

func (t *Transformer) transformElem(parent *ast.Ident, x astx.Elem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformElem"))
	}
	if x == nil {
		return nil
	}

	switch elem := x.(type) {
	case *astx.ForElem:
		return t.transformForElem(parent, elem)
	case *astx.RangeElem:
		return t.transformRangeElem(parent, elem)
	case *astx.IfElem:
		return t.transformIfElem(parent, elem)
	case *astx.HtmlElem:
		return t.transformHtmlElem(parent, elem)
	case *astx.BlockElem:
		return t.tranformBlockElem(parent, elem)
	case *astx.ElemCaseClause:
		return t.transformElemCaseClause(parent, elem)
	case *astx.SwitchElem:
		return t.transformSwitchElem(parent, elem)
	case *astx.TypeSwitchElem:
		return t.transformTypeSwitchElem(parent, elem)
	case *astx.TextElem:
		return t.transformTextElem(parent, elem)
	case *astx.ElemCall:
		return t.transformElemCall(parent, elem)
	case *astx.BadElem:
		return &ast.BlockStmt{}
	default:
		return &ast.BlockStmt{}
	}
}

func (t *Transformer) tranformBlockElem(parent *ast.Ident, block *astx.BlockElem) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "tranformBlockElem"))
	}
	var list []ast.Stmt
	for _, elem := range block.List {
		items := t.transformElem(parent, elem)
		list = append(list, items.List...)
	}
	return &ast.BlockStmt{List: list}
}
