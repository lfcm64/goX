package transformer

import (
	"go/ast"
	"goX/transpiler/astx"
)

func (t *Transformer) transformBadStmt(x *astx.BadStmt) *ast.BadStmt {
	if t.trace {
		defer un(trace(t, "transformBadStmt"))
	}
	if x == nil {
		return nil
	}
	return &ast.BadStmt{
		From: x.From,
		To:   x.To,
	}
}

func (t *Transformer) transformDeclStmt(x *astx.DeclStmt) *ast.DeclStmt {
	if t.trace {
		defer un(trace(t, "transformDeclStmt"))
	}
	if x == nil {
		return nil
	}
	return &ast.DeclStmt{Decl: t.transformDecl(x.Decl)}
}

func (t *Transformer) transformEmptyStmt(x *astx.EmptyStmt) *ast.EmptyStmt {
	if t.trace {
		defer un(trace(t, "transformEmptyStmt"))
	}
	if x == nil {
		return nil
	}
	return &ast.EmptyStmt{
		Semicolon: x.Semicolon,
		Implicit:  x.Implicit,
	}
}

func (t *Transformer) transformLabeledStmt(x *astx.LabeledStmt) *ast.LabeledStmt {
	if t.trace {
		defer un(trace(t, "transformLabeledStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.LabeledStmt{
		Label: t.transformIdent(x.Label),
		Colon: x.Colon,
		Stmt:  t.transformStmt(x.Stmt),
	}
}

func (t *Transformer) transformExprStmt(x *astx.ExprStmt) *ast.ExprStmt {
	if t.trace {
		defer un(trace(t, "transformExprStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.ExprStmt{X: t.transformExpr(x.X)}
}

func (t *Transformer) transformSendStmt(x *astx.SendStmt) *ast.SendStmt {
	if t.trace {
		defer un(trace(t, "transformSendStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.SendStmt{
		Chan:  t.transformExpr(x.Chan),
		Arrow: x.Arrow,
		Value: t.transformExpr(x.Value),
	}
}

func (t *Transformer) transformIncDecStmt(x *astx.IncDecStmt) *ast.IncDecStmt {
	if t.trace {
		defer un(trace(t, "transformIncDecStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.IncDecStmt{
		X:      t.transformExpr(x.X),
		TokPos: x.TokPos,
		Tok:    x.Tok,
	}
}

func (t *Transformer) transformAssignStmt(x *astx.AssignStmt) *ast.AssignStmt {
	if t.trace {
		defer un(trace(t, "transformAssignStmt"))
	}
	if x == nil {
		return nil
	}

	lhs := []ast.Expr{}
	for _, l := range x.Lhs {
		lhs = append(lhs, t.transformExpr(l))
	}

	rhs := []ast.Expr{}
	for _, r := range x.Rhs {
		rhs = append(rhs, t.transformExpr(r))
	}

	return &ast.AssignStmt{
		Lhs:    lhs,
		TokPos: x.TokPos,
		Tok:    x.Tok,
		Rhs:    rhs,
	}
}

func (t *Transformer) transformGoStmt(x *astx.GoStmt) *ast.GoStmt {
	if t.trace {
		defer un(trace(t, "transformGoStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.GoStmt{
		Go:   x.Go,
		Call: t.transformCallExpr(x.Call),
	}
}

func (t *Transformer) transformDeferStmt(x *astx.DeferStmt) *ast.DeferStmt {
	if t.trace {
		defer un(trace(t, "transformDeferStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.DeferStmt{
		Defer: x.Defer,
		Call:  t.transformCallExpr(x.Call),
	}
}

func (t *Transformer) transformReturnStmt(x *astx.ReturnStmt) *ast.ReturnStmt {
	if t.trace {
		defer un(trace(t, "transformReturnStmt"))
	}
	if x == nil {
		return nil
	}

	var results []ast.Expr
	for _, res := range x.Results {
		results = append(results, t.transformExpr(res))
	}

	return &ast.ReturnStmt{
		Return:  x.Return,
		Results: results,
	}
}

func (t *Transformer) transformBranchStmt(x *astx.BranchStmt) *ast.BranchStmt {
	if t.trace {
		defer un(trace(t, "transformBranchStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.BranchStmt{
		TokPos: x.TokPos,
		Tok:    x.Tok,
		Label:  t.transformIdent(x.Label),
	}
}

func (t *Transformer) transformBlockStmt(x *astx.BlockStmt) *ast.BlockStmt {
	if t.trace {
		defer un(trace(t, "transformBlockStmt"))
	}
	if x == nil {
		return nil
	}

	var list []ast.Stmt
	for _, stmt := range x.List {
		list = append(list, t.transformStmt(stmt))
	}

	return &ast.BlockStmt{
		Lbrace: x.Lbrace,
		List:   list,
		Rbrace: x.Rbrace,
	}
}

func (t *Transformer) transformIfStmt(x *astx.IfStmt) *ast.IfStmt {
	if t.trace {
		defer un(trace(t, "transformIfStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.IfStmt{
		If:   x.If,
		Init: t.transformStmt(x.Init),
		Cond: t.transformExpr(x.Cond),
		Body: t.transformBlockStmt(x.Body),
		Else: t.transformStmt(x.Else),
	}
}

func (t *Transformer) transformCaseClause(x *astx.CaseClause) *ast.CaseClause {
	if t.trace {
		defer un(trace(t, "transformCaseClause"))
	}
	if x == nil {
		return nil
	}

	var list []ast.Expr
	for _, expr := range x.List {
		list = append(list, t.transformExpr(expr))
	}

	var body []ast.Stmt
	for _, stmt := range x.Body {
		body = append(body, t.transformStmt(stmt))
	}

	return &ast.CaseClause{
		Case:  x.Case,
		List:  list,
		Colon: x.Colon,
		Body:  body,
	}
}

func (t *Transformer) transformSwitchStmt(x *astx.SwitchStmt) *ast.SwitchStmt {
	if t.trace {
		defer un(trace(t, "transformSwitchStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.SwitchStmt{
		Switch: x.Switch,
		Init:   t.transformStmt(x.Init),
		Tag:    t.transformExpr(x.Tag),
		Body:   t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformTypeSwitchStmt(x *astx.TypeSwitchStmt) *ast.TypeSwitchStmt {
	if t.trace {
		defer un(trace(t, "transformTypeSwitchStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.TypeSwitchStmt{
		Switch: x.Switch,
		Init:   t.transformStmt(x.Init),
		Assign: t.transformStmt(x.Assign),
		Body:   t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformCommClause(x *astx.CommClause) *ast.CommClause {
	if t.trace {
		defer un(trace(t, "transformCommClause"))
	}
	if x == nil {
		return nil
	}

	var body []ast.Stmt
	for _, stmt := range x.Body {
		body = append(body, t.transformStmt(stmt))
	}

	return &ast.CommClause{
		Case:  x.Case,
		Comm:  t.transformStmt(x.Comm),
		Colon: x.Colon,
		Body:  body,
	}
}

func (t *Transformer) transformSelectStmt(x *astx.SelectStmt) *ast.SelectStmt {
	if t.trace {
		defer un(trace(t, "transformSelectStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.SelectStmt{
		Select: x.Select,
		Body:   t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformForStmt(x *astx.ForStmt) *ast.ForStmt {
	if t.trace {
		defer un(trace(t, "transformForStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.ForStmt{
		For:  x.For,
		Init: t.transformStmt(x.Init),
		Cond: t.transformExpr(x.Cond),
		Post: t.transformStmt(x.Post),
		Body: t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformRangeStmt(x *astx.RangeStmt) *ast.RangeStmt {
	if t.trace {
		defer un(trace(t, "transformRangeStmt"))
	}
	if x == nil {
		return nil
	}

	return &ast.RangeStmt{
		For:    x.For,
		Key:    t.transformExpr(x.Key),
		Value:  t.transformExpr(x.Value),
		TokPos: x.TokPos,
		Tok:    x.Tok,
		Range:  x.Range,
		X:      t.transformExpr(x.X),
		Body:   t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformStmt(x astx.Stmt) ast.Stmt {
	if t.trace {
		defer un(trace(t, "transformStmt"))
	}
	if x == nil {
		return nil
	}
	switch stmt := x.(type) {
	case *astx.BadStmt:
		return t.transformBadStmt(stmt)
	case *astx.DeclStmt:
		return t.transformDeclStmt(stmt)
	case *astx.EmptyStmt:
		return t.transformEmptyStmt(stmt)
	case *astx.LabeledStmt:
		return t.transformLabeledStmt(stmt)
	case *astx.ExprStmt:
		return t.transformExprStmt(stmt)
	case *astx.SendStmt:
		return t.transformSendStmt(stmt)
	case *astx.IncDecStmt:
		return t.transformIncDecStmt(stmt)
	case *astx.AssignStmt:
		return t.transformAssignStmt(stmt)
	case *astx.GoStmt:
		return t.transformGoStmt(stmt)
	case *astx.DeferStmt:
		return t.transformDeferStmt(stmt)
	case *astx.ReturnStmt:
		return t.transformReturnStmt(stmt)
	case *astx.BranchStmt:
		return t.transformBranchStmt(stmt)
	case *astx.BlockStmt:
		return t.transformBlockStmt(stmt)
	case *astx.IfStmt:
		return t.transformIfStmt(stmt)
	case *astx.CaseClause:
		return t.transformCaseClause(stmt)
	case *astx.SwitchStmt:
		return t.transformSwitchStmt(stmt)
	case *astx.TypeSwitchStmt:
		return t.transformTypeSwitchStmt(stmt)
	case *astx.CommClause:
		return t.transformCommClause(stmt)
	case *astx.SelectStmt:
		return t.transformSelectStmt(stmt)
	case *astx.ForStmt:
		return t.transformForStmt(stmt)
	case *astx.RangeStmt:
		return t.transformRangeStmt(stmt)
	default:
		return nil
	}
}
