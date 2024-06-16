package parser

import (
	"fmt"
	"go/token"
	"goX/transpiler/astx"
	"strings"
)

func filterText(text string) string {
	return strings.TrimSpace(text)
}

var elemStart = map[token.Token]bool{
	token.FOR:    true,
	token.IF:     true,
	token.LSS:    true,
	token.IDENT:  true,
	token.LBRACE: true,
	token.SWITCH: true,
}

func (p *Parser) parseOpeningTag() (ot *astx.OpeningTag, sc bool) {
	if p.trace {
		defer un(trace(p, "OpeningTag"))
	}

	langle := p.pos - 1
	name := p.parseIdent()

	if p.tok == token.QUO {
		p.next()
		sc = true
	}
	rangle := p.expect(token.GTR)
	ot = &astx.OpeningTag{Langle: langle, Name: name, Rangle: rangle}
	return
}

func (p *Parser) parseClosingTag() *astx.ClosingTag {
	if p.trace {
		defer un(trace(p, "ClosingTag"))
	}

	langle := p.pos - 1
	p.expect(token.QUO)

	name := p.parseIdent()

	rangle := p.expect(token.GTR)
	return &astx.ClosingTag{Langle: langle, Name: name, Rangle: rangle}
}

func (p *Parser) parseTextString() *astx.BasicLit {
	bl := &astx.BasicLit{ValuePos: p.pos, Kind: token.STRING}
	var accum string
	for ; p.tok != token.EOF; p.next() {
		if elemStart[p.tok] && p.tok != token.IDENT {
			break
		}
		if p.tok == token.IDENT {
			accum += p.lit
			continue
		}
		accum += p.tok.String()
	}
	bl.Value = fmt.Sprintf("\"%s\"", accum)
	return bl
}

func (p *Parser) parseTextExpr() astx.Expr {
	p.expect(token.LBRACE)
	expr := p.parseExpr()
	p.expect(token.RBRACE)
	return expr
}

func (p *Parser) parseTextElem() astx.Elem {
	pos := p.pos
	var list []astx.Expr

	for p.tok != token.EOF {
		if elemStart[p.tok] && p.tok != token.IDENT && p.tok != token.LBRACE {
			break
		} else if p.tok == token.LBRACE {
			list = append(list, p.parseTextExpr())
		} else {
			text := p.parseTextString()
			if text.Value != "\"\"" {
				list = append(list, text)
			}
		}
	}

	return &astx.TextElem{L: pos, List: list, R: p.pos}
}

func (p *Parser) parseForElement() astx.Elem {
	if p.trace {
		defer un(trace(p, "ForElem"))
	}
	pos := p.expect(token.FOR)

	var s1, s2, s3 astx.Stmt
	var isRange bool
	if p.tok != token.LBRACE {
		prevLev := p.exprLev
		p.exprLev = -1
		if p.tok != token.SEMICOLON {
			if p.tok == token.RANGE {
				// "for range x" (nil lhs in assignment)
				pos := p.pos
				p.next()
				y := []astx.Expr{&astx.UnaryExpr{OpPos: pos, Op: token.RANGE, X: p.parseRhs()}}
				s2 = &astx.AssignStmt{Rhs: y}
				isRange = true
			} else {
				s2, isRange = p.parseSimpleStmt(rangeOk)
			}
		}
		if !isRange && p.tok == token.SEMICOLON {
			p.next()

			s1 = s2
			s2 = nil
			if p.tok != token.SEMICOLON {
				s2, _ = p.parseSimpleStmt(basic)
			}
			p.expectSemi()
			if p.tok != token.LBRACE {
				s3, _ = p.parseSimpleStmt(basic)
			}
		}
		p.exprLev = prevLev
	}
	p.next()

	body := p.parseBlockElem()

	p.expect(token.RBRACE)
	p.expectSemi()

	if isRange {
		as := s2.(*astx.AssignStmt)
		// check lhs
		var key, value astx.Expr
		switch len(as.Lhs) {
		case 0:
			// nothing to do
		case 1:
			key = as.Lhs[0]
		case 2:
			key, value = as.Lhs[0], as.Lhs[1]
		default:
			p.errorExpected(as.Lhs[len(as.Lhs)-1].Pos(), "at most 2 expressions")
			return &astx.BadElem{From: pos, To: p.safePos(body.End())}
		}
		// parseSimpleStmt returned a right-hand side that
		// is a single unary expression of the form "range x"
		x := as.Rhs[0].(*astx.UnaryExpr).X
		return &astx.RangeElem{
			For:    pos,
			Key:    key,
			Value:  value,
			TokPos: as.TokPos,
			Tok:    as.Tok,
			Range:  as.Rhs[0].Pos(),
			X:      x,
			Body:   body,
		}
	}

	// regular for statement
	return &astx.ForElem{
		For:  pos,
		Init: s1,
		Cond: p.makeExpr(s2, "boolean or range expression"),
		Post: s3,
		Body: body,
	}
}

func (p *Parser) parseIfElement() astx.Elem {
	defer decNestLev(incNestLev(p))

	if p.trace {
		defer un(trace(p, "IfElem"))
	}

	pos := p.expect(token.IF)

	init, cond := p.parseIfHeader()
	p.next()
	body := p.parseBlockElem()
	p.expect(token.RBRACE)

	var else_ astx.Elem
	if p.tok == token.ELSE {
		p.next()
		switch p.tok {
		case token.IF:
			else_ = p.parseIfElement()
		case token.LBRACE:
			else_ = p.parseBlockElem()
			p.expectSemi()
		default:
			p.errorExpected(p.pos, "if element or block")
			else_ = &astx.BadElem{From: p.pos, To: p.pos}
		}
	} else {
		p.expectSemi()
	}

	return &astx.IfElem{If: pos, Init: init, Cond: cond, Body: body, Else: else_}
}

func (p *Parser) parseElemCaseClause() *astx.ElemCaseClause {
	if p.trace {
		defer un(trace(p, "ElemCaseClause"))
	}

	pos := p.pos
	var list []astx.Expr
	if p.tok == token.CASE {
		p.next()
		list = p.parseList(true)
	} else {
		p.expect(token.DEFAULT)
	}

	colon := p.expect(token.COLON)
	body := p.parseBlockElem()

	return &astx.ElemCaseClause{Case: pos, List: list, Colon: colon, Body: body}
}

func (p *Parser) parseSwitchElem() astx.Elem {
	if p.trace {
		defer un(trace(p, "SwitchElem"))
	}

	pos := p.expect(token.SWITCH)

	var s1, s2 astx.Stmt
	if p.tok != token.LBRACE {
		prevLev := p.exprLev
		p.exprLev = -1
		if p.tok != token.SEMICOLON {
			s2, _ = p.parseSimpleStmt(basic)
		}
		if p.tok == token.SEMICOLON {
			p.next()
			s1 = s2
			s2 = nil
			if p.tok != token.LBRACE {
				s2, _ = p.parseSimpleStmt(basic)
			}
		}
		p.exprLev = prevLev
	}

	typeSwitch := p.isTypeSwitchGuard(s2)
	lbrace := p.expect(token.LBRACE)
	var list []astx.Elem

	for p.tok == token.CASE || p.tok == token.DEFAULT {
		list = append(list, p.parseElemCaseClause())
	}

	rbrace := p.expect(token.RBRACE)
	p.expectSemi()
	body := &astx.BlockElem{L: lbrace, List: list, R: rbrace}

	if typeSwitch {
		return &astx.TypeSwitchElem{Switch: pos, Init: s1, Assign: s2, Body: body}
	}

	return &astx.SwitchElem{Switch: pos, Init: s1, Tag: p.makeExpr(s2, "switch expression"), Body: body}
}

func (p *Parser) parseElemCall() astx.Elem {
	rem := p.expect(token.REM)
	call := p.parseCallExpr("%")
	p.expectSemi()
	return &astx.ElemCall{Rem: rem, Call: call}
}

func (p *Parser) parseElem(lss bool) astx.Elem {
	pos := p.pos

	if lss {
		return p.parseHtml()
	}
	switch p.tok {
	case token.FOR:
		return p.parseForElement()
	case token.IF:
		return p.parseIfElement()
	case token.IDENT, token.LBRACE:
		return p.parseTextElem()
	case token.SWITCH:
		return p.parseSwitchElem()
	case token.REM:
		return p.parseElemCall()
	default:
		p.advance(elemStart)
		return &astx.BadElem{From: pos, To: p.pos}
	}
}

func (p *Parser) parseElemList() (list []astx.Elem) {
	if p.trace {
		defer un(trace(p, "ElemList"))
	}

	for p.tok != token.RBRACE && p.tok != token.CASE && p.tok != token.DEFAULT && p.tok != token.EOF {
		rangle := false
		if p.tok == token.LSS {
			p.next()

			if p.tok == token.QUO {
				return
			}
			rangle = true
		}
		list = append(list, p.parseElem(rangle))
	}
	return
}

func (p *Parser) parseBlockElem() *astx.BlockElem {
	if p.trace {
		defer un(trace(p, "BlockElem"))
	}

	pos := p.pos
	list := p.parseElemList()

	return &astx.BlockElem{L: pos, List: list, R: p.pos}
}

func (p *Parser) parseHtml() *astx.HtmlElem {
	op, sc := p.parseOpeningTag()
	body := p.parseBlockElem()
	var cl *astx.ClosingTag
	if !sc {
		cl = p.parseClosingTag()
	}
	p.elemEnd = true
	return &astx.HtmlElem{Opening: op, Body: body, SelfClosing: sc, Closing: cl}
}
