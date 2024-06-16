package transformer

import (
	"go/ast"
	"goX/transpiler/astx"
)

func (t *Transformer) transformBadExpr(x *astx.BadExpr) *ast.BadExpr {
	if t.trace {
		defer un(trace(t, "transformBadExpr"))
	}
	if x == nil {
		return nil
	}
	return &ast.BadExpr{
		From: x.From,
		To:   x.To,
	}
}

func (t *Transformer) transformIdent(x *astx.Ident) *ast.Ident {
	if x == nil {
		return nil
	}
	if t.trace {
		defer t.printTrace("IDENT \"" + x.Name + "\"")
	}
	return &ast.Ident{
		NamePos: x.NamePos,
		Name:    x.Name,
		Obj:     x.Obj,
	}
}

func (t *Transformer) transformEllipsis(x *astx.Ellipsis) *ast.Ellipsis {
	if t.trace {
		defer un(trace(t, "transformEllipsis"))
	}
	if x == nil {
		return nil
	}

	return &ast.Ellipsis{
		Ellipsis: x.Ellipsis,
		Elt:      t.transformExpr(x.Elt),
	}
}

func (t *Transformer) transformBasicLit(x *astx.BasicLit) *ast.BasicLit {
	if x == nil {
		return nil
	}
	if t.trace {
		defer t.printTrace(x.Kind.String() + " " + x.Value)
	}

	return &ast.BasicLit{
		ValuePos: x.ValuePos,
		Kind:     x.Kind,
		Value:    x.Value,
	}
}

func (t *Transformer) transformFuncLit(x *astx.FuncLit) *ast.FuncLit {
	if t.trace {
		defer un(trace(t, "transformFuncLit"))
	}
	if x == nil {
		return nil
	}

	return &ast.FuncLit{
		Type: t.transformFuncType(x.Type),
		Body: t.transformBlockStmt(x.Body),
	}
}

func (t *Transformer) transformCompositeLit(x *astx.CompositeLit) *ast.CompositeLit {
	if t.trace {
		defer un(trace(t, "transformCompositeLit"))
	}
	if x == nil {
		return nil
	}

	elts := []ast.Expr{}
	for _, elt := range x.Elts {
		elts = append(elts, t.transformExpr(elt))
	}

	return &ast.CompositeLit{
		Type:       t.transformExpr(x.Type),
		Lbrace:     x.Lbrace,
		Elts:       elts,
		Rbrace:     x.Rbrace,
		Incomplete: x.Incomplete,
	}
}

func (t *Transformer) transformParenExpr(x *astx.ParenExpr) *ast.ParenExpr {
	if t.trace {
		defer un(trace(t, "transformParenExpr"))
	}
	if x == nil {
		return nil
	}
	return &ast.ParenExpr{
		Lparen: x.Lparen,
		X:      t.transformExpr(x.X),
		Rparen: x.Rparen,
	}
}

func (t *Transformer) transformSelectorExpr(x *astx.SelectorExpr) *ast.SelectorExpr {
	if t.trace {
		defer un(trace(t, "transformSelectorExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.SelectorExpr{
		X:   t.transformExpr(x.X),
		Sel: t.transformIdent(x.Sel),
	}
}

func (t *Transformer) transformIndexExpr(x *astx.IndexExpr) *ast.IndexExpr {
	if t.trace {
		defer un(trace(t, "transformIndexExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.IndexExpr{
		X:      t.transformExpr(x.X),
		Lbrack: x.Lbrack,
		Index:  t.transformExpr(x.Index),
		Rbrack: x.Rbrack,
	}
}

func (t *Transformer) transformIndexListExpr(x *astx.IndexListExpr) *ast.IndexListExpr {
	if t.trace {
		defer un(trace(t, "transformIndexListExpr"))
	}
	if x == nil {
		return nil
	}

	var indices []ast.Expr
	for _, indice := range x.Indices {
		indices = append(indices, t.transformExpr(indice))
	}

	return &ast.IndexListExpr{
		X:       t.transformExpr(x.X),
		Lbrack:  x.Lbrack,
		Indices: indices,
		Rbrack:  x.Rbrack,
	}
}

func (t *Transformer) transformSliceExpr(x *astx.SliceExpr) *ast.SliceExpr {
	if t.trace {
		defer un(trace(t, "transformSliceExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.SliceExpr{
		X:      t.transformExpr(x.X),
		Lbrack: x.Lbrack,
		Low:    t.transformExpr(x.Low),
		High:   t.transformExpr(x.High),
		Max:    t.transformExpr(x.Max),
		Slice3: x.Slice3,
		Rbrack: x.Rbrack,
	}
}

func (t *Transformer) transformTypeAssertExpr(x *astx.TypeAssertExpr) *ast.TypeAssertExpr {
	if t.trace {
		defer un(trace(t, "transformTypeAssertExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.TypeAssertExpr{
		X:      t.transformExpr(x.X),
		Lparen: x.Lparen,
		Type:   t.transformExpr(x.Type),
		Rparen: x.Rparen,
	}
}

func (t *Transformer) transformCallExpr(x *astx.CallExpr) *ast.CallExpr {
	if t.trace {
		defer un(trace(t, "transformCallExpr"))
	}
	if x == nil {
		return nil
	}

	var args []ast.Expr
	for _, arg := range x.Args {
		args = append(args, t.transformExpr(arg))
	}

	return &ast.CallExpr{
		Fun:      t.transformExpr(x.Fun),
		Lparen:   x.Lparen,
		Args:     args,
		Ellipsis: x.Ellipsis,
		Rparen:   x.Rparen,
	}
}

func (t *Transformer) transformStarExpr(x *astx.StarExpr) *ast.StarExpr {
	if t.trace {
		defer un(trace(t, "transformStarExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.StarExpr{
		Star: x.Star,
		X:    t.transformExpr(x.X),
	}
}

func (t *Transformer) transformUnaryExpr(x *astx.UnaryExpr) *ast.UnaryExpr {
	if t.trace {
		defer un(trace(t, "transformUnaryExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.UnaryExpr{
		OpPos: x.OpPos,
		Op:    x.Op,
		X:     t.transformExpr(x.X),
	}
}

func (t *Transformer) transformBinaryExpr(x *astx.BinaryExpr) *ast.BinaryExpr {
	if t.trace {
		defer un(trace(t, "transformBinaryExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.BinaryExpr{
		X:     t.transformExpr(x.X),
		OpPos: x.OpPos,
		Op:    x.Op,
		Y:     t.transformExpr(x.Y),
	}
}

func (t *Transformer) transformKeyValueExpr(x *astx.KeyValueExpr) *ast.KeyValueExpr {
	if t.trace {
		defer un(trace(t, "transformKeyValueExpr"))
	}
	if x == nil {
		return nil
	}

	return &ast.KeyValueExpr{
		Key:   t.transformExpr(x.Key),
		Colon: x.Colon,
		Value: t.transformExpr(x.Value),
	}
}

func (t *Transformer) transformArrayType(x *astx.ArrayType) *ast.ArrayType {
	if t.trace {
		defer un(trace(t, "transformArrayType"))
	}
	if x == nil {
		return nil
	}

	return &ast.ArrayType{
		Lbrack: x.Lbrack,
		Len:    t.transformExpr(x.Len),
		Elt:    t.transformExpr(x.Elt),
	}
}

func (t *Transformer) transformStructType(x *astx.StructType) *ast.StructType {
	if t.trace {
		defer un(trace(t, "transformStructType"))
	}
	if x == nil {
		return nil
	}

	return &ast.StructType{
		Struct:     x.Struct,
		Fields:     t.transformFieldList(x.Fields),
		Incomplete: x.Incomplete,
	}
}

func (t *Transformer) transformFuncType(x *astx.FuncType) *ast.FuncType {
	if t.trace {
		defer un(trace(t, "transformFuncType"))
	}
	if x == nil {
		return nil
	}

	return &ast.FuncType{
		Func:       x.Func,
		TypeParams: t.transformFieldList(x.TypeParams),
		Params:     t.transformFieldList(x.Params),
		Results:    t.transformFieldList(x.Results),
	}
}

func (t *Transformer) transformInterfaceType(x *astx.InterfaceType) *ast.InterfaceType {
	if t.trace {
		defer un(trace(t, "transformInterfaceType"))
	}
	if x == nil {
		return nil
	}

	return &ast.InterfaceType{
		Interface:  x.Interface,
		Methods:    t.transformFieldList(x.Methods),
		Incomplete: x.Incomplete,
	}
}

func (t *Transformer) transformMapType(x *astx.MapType) *ast.MapType {
	if t.trace {
		defer un(trace(t, "transformMapType"))
	}
	if x == nil {
		return nil
	}

	return &ast.MapType{
		Map:   x.Map,
		Key:   t.transformExpr(x.Key),
		Value: t.transformExpr(x.Value),
	}
}

func (t *Transformer) transformChanType(x *astx.ChanType) *ast.ChanType {
	if t.trace {
		defer un(trace(t, "transformChanType"))
	}
	if x == nil {
		return nil
	}

	return &ast.ChanType{
		Begin: x.Begin,
		Arrow: x.Arrow,
		Dir:   ast.ChanDir(x.Dir),
		Value: t.transformExpr(x.Value),
	}
}

func (t *Transformer) transformHtmlType(x *astx.Ident) *ast.SelectorExpr {
	pos := x.Pos()
	return &ast.SelectorExpr{
		X: &ast.Ident{
			NamePos: pos,
			Name:    "gox",
		},
		Sel: &ast.Ident{
			NamePos: pos + 4,
			Name:    "Component",
		},
	}
}

func (t *Transformer) transformExpr(x astx.Expr) ast.Expr {
	if t.trace {
		defer un(trace(t, "transformExpr"))
	}
	if x == nil {
		return nil
	}

	switch expr := x.(type) {
	case *astx.Ident:
		if expr.Name == "html" {
			return t.transformHtmlType(expr)
		}
		return t.transformIdent(expr)
	case *astx.BadExpr:
		return t.transformBadExpr(expr)
	case *astx.Ellipsis:
		return t.transformEllipsis(expr)
	case *astx.BasicLit:
		return t.transformBasicLit(expr)
	case *astx.FuncLit:
		return t.transformFuncLit(expr)
	case *astx.CompositeLit:
		return t.transformCompositeLit(expr)
	case *astx.ParenExpr:
		return t.transformParenExpr(expr)
	case *astx.SelectorExpr:
		return t.transformSelectorExpr(expr)
	case *astx.IndexExpr:
		return t.transformIndexExpr(expr)
	case *astx.IndexListExpr:
		return t.transformIndexListExpr(expr)
	case *astx.SliceExpr:
		return t.transformSliceExpr(expr)
	case *astx.TypeAssertExpr:
		return t.transformTypeAssertExpr(expr)
	case *astx.CallExpr:
		return t.transformCallExpr(expr)
	case *astx.StarExpr:
		return t.transformStarExpr(expr)
	case *astx.UnaryExpr:
		return t.transformUnaryExpr(expr)
	case *astx.BinaryExpr:
		return t.transformBinaryExpr(expr)
	case *astx.KeyValueExpr:
		return t.transformKeyValueExpr(expr)
	case *astx.ArrayType:
		return t.transformArrayType(expr)
	case *astx.StructType:
		return t.transformStructType(expr)
	case *astx.FuncType:
		return t.transformFuncType(expr)
	case *astx.InterfaceType:
		return t.transformInterfaceType(expr)
	case *astx.MapType:
		return t.transformMapType(expr)
	case *astx.ChanType:
		return t.transformChanType(expr)
	case *astx.HtmlElem:
		return t.transformHtmlExpr(expr)
	default:
		return nil
	}
}
