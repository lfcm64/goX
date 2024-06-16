package astx

import "go/token"

type Elem interface {
	Node
	elemNode()
}

type (
	OpeningTag struct {
		Langle token.Pos
		Name   *Ident
		Rangle token.Pos
	}

	ClosingTag struct {
		Langle token.Pos //position of '<'
		Name   *Ident
		Rangle token.Pos //position of '>'
	}
)

func (x *OpeningTag) Pos() token.Pos { return x.Langle }
func (x *ClosingTag) Pos() token.Pos { return x.Langle }

func (x *OpeningTag) End() token.Pos { return x.Rangle }
func (x *ClosingTag) End() token.Pos { return x.Rangle }

type (
	BadElem struct {
		From, To token.Pos // position range of bad statement
	}

	TextElem struct {
		L    token.Pos
		List []Expr
		R    token.Pos
	}

	BlockElem struct {
		L    token.Pos
		List []Elem
		R    token.Pos
	}

	HtmlElem struct {
		Opening     *OpeningTag
		Body        *BlockElem //nil if self-closing element
		SelfClosing bool
		Closing     *ClosingTag //nil if self-closing element
	}

	ForElem struct {
		For  token.Pos // position of "for" keyword
		Init Stmt      // initialization statement; or nil
		Cond Expr      // condition; or nil
		Post Stmt      // post iteration statement; or nil
		Body *BlockElem
	}

	RangeElem struct {
		For        token.Pos   // position of "for" keyword
		Key, Value Expr        // Key, Value may be nil
		TokPos     token.Pos   // position of Tok; invalid if Key == nil
		Tok        token.Token // ILLEGAL if Key == nil, ASSIGN, DEFINE
		Range      token.Pos   // position of "range" keyword
		X          Expr        // value to range over
		Body       *BlockElem
	}

	IfElem struct {
		If   token.Pos // position of "if" keyword
		Init Stmt      // initialization statement; or nil
		Cond Expr      // condition
		Body *BlockElem
		Else Elem // else branch; or nil
	}

	ElemCaseClause struct {
		Case  token.Pos  // position of "case" or "default" keyword
		List  []Expr     // list of expressions or types; nil means default case
		Colon token.Pos  // position of ":"
		Body  *BlockElem // statement list; or nil
	}

	SwitchElem struct {
		Switch token.Pos  // position of "switch" keyword
		Init   Stmt       // initialization statement; or nil
		Tag    Expr       // tag expression; or nil
		Body   *BlockElem // CaseClauses only
	}

	TypeSwitchElem struct {
		Switch token.Pos  // position of "switch" keyword
		Init   Stmt       // initialization statement; or nil
		Assign Stmt       // x := y.(type) or y.(type)
		Body   *BlockElem // CaseClauses only
	}

	ElemCall struct {
		Rem  token.Pos //position of "%" keyword
		Call *CallExpr //call expression
	}
)

func (x *BadElem) Pos() token.Pos        { return x.From }
func (x *TextElem) Pos() token.Pos       { return x.L }
func (x *BlockElem) Pos() token.Pos      { return x.L }
func (x *HtmlElem) Pos() token.Pos       { return x.Opening.Pos() }
func (x *ForElem) Pos() token.Pos        { return x.For }
func (x *RangeElem) Pos() token.Pos      { return x.For }
func (x *IfElem) Pos() token.Pos         { return x.If }
func (x *ElemCaseClause) Pos() token.Pos { return x.Case }
func (x *SwitchElem) Pos() token.Pos     { return x.Switch }
func (x *TypeSwitchElem) Pos() token.Pos { return x.Switch }
func (x *ElemCall) Pos() token.Pos       { return x.Rem }

func (x *BadElem) End() token.Pos   { return x.To }
func (x *TextElem) End() token.Pos  { return x.R }
func (x *BlockElem) End() token.Pos { return x.R }
func (x *HtmlElem) End() token.Pos  { return x.Opening.Pos() }
func (x *ForElem) End() token.Pos   { return x.Body.End() }
func (x *RangeElem) End() token.Pos { return x.Body.End() }
func (x *IfElem) End() token.Pos {
	if x.Else != nil {
		return x.Else.End()
	}
	return x.Body.End()
}
func (x *ElemCaseClause) End() token.Pos { return x.Body.End() }
func (x *SwitchElem) End() token.Pos     { return x.Body.End() }
func (x *TypeSwitchElem) End() token.Pos { return x.Body.End() }
func (x *ElemCall) End() token.Pos       { return x.Call.End() }

func (*HtmlElem) exprNode() {}

func (*BadElem) elemNode()        {}
func (*TextElem) elemNode()       {}
func (*BlockElem) elemNode()      {}
func (*HtmlElem) elemNode()       {}
func (*ForElem) elemNode()        {}
func (*RangeElem) elemNode()      {}
func (*IfElem) elemNode()         {}
func (*ElemCaseClause) elemNode() {}
func (*SwitchElem) elemNode()     {}
func (*TypeSwitchElem) elemNode() {}
func (*ElemCall) elemNode()       {}
