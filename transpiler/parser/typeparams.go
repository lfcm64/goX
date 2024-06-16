// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parser

import (
	"go/token"
	"goX/transpiler/astx"
)

func PackIndexExpr(x astx.Expr, lbrack token.Pos, exprs []astx.Expr, rbrack token.Pos) astx.Expr {
	switch len(exprs) {
	case 0:
		panic("internal error: PackIndexExpr with empty expr slice")
	case 1:
		return &astx.IndexExpr{
			X:      x,
			Lbrack: lbrack,
			Index:  exprs[0],
			Rbrack: rbrack,
		}
	default:
		return &astx.IndexListExpr{
			X:       x,
			Lbrack:  lbrack,
			Indices: exprs,
			Rbrack:  rbrack,
		}
	}
}

// IndexExpr wraps an astx.IndexExpr or astx.IndexListExpr.
//
// Orig holds the original astx.Expr from which this IndexExpr was derived.
//
// Note: IndexExpr (intentionally) does not wrap astx.Expr, as that leads to
// accidental misuse such as encountered in golang/go#63933.
//
// TODO(rfindley): remove this helper, in favor of just having a helper
// function that returns indices.
type IndexExpr struct {
	Orig    astx.Expr   // the wrapped expr, which may be distinct from the IndexListExpr below.
	X       astx.Expr   // expression
	Lbrack  token.Pos   // position of "["
	Indices []astx.Expr // index expressions
	Rbrack  token.Pos   // position of "]"
}

func (x *IndexExpr) Pos() token.Pos {
	return x.Orig.Pos()
}

func UnpackIndexExpr(n astx.Node) *IndexExpr {
	switch e := n.(type) {
	case *astx.IndexExpr:
		return &IndexExpr{
			Orig:    e,
			X:       e.X,
			Lbrack:  e.Lbrack,
			Indices: []astx.Expr{e.Index},
			Rbrack:  e.Rbrack,
		}
	case *astx.IndexListExpr:
		return &IndexExpr{
			Orig:    e,
			X:       e.X,
			Lbrack:  e.Lbrack,
			Indices: e.Indices,
			Rbrack:  e.Rbrack,
		}
	}
	return nil
}
