package transpiler

import (
	"fmt"
	"go/scanner"
	"go/token"
	"goX/transpiler/astx"
)

type analyser struct {
	tfile *token.File
	file  *astx.File
	eh    scanner.ErrorHandler
}

func (a *analyser) init(tfile *token.File, file *astx.File, eh scanner.ErrorHandler) {
	a.tfile = tfile
	a.file = file
	a.eh = eh
}

func (a *analyser) analyseFile() {
	for _, decl := range a.file.Decls {
		astx.Walk(a, decl)
	}
}

func (a *analyser) analyseHtmlElem(elem *astx.HtmlElem) {
	epos := a.tfile.Position(elem.Pos())

	if elem.SelfClosing {
		if elem.Body != nil {
			a.eh(epos, "Self closing element has children")
		}
		if elem.Closing != nil {
			a.eh(epos, "Self closing element has a closing tag")
		}
		a.checkSelfClosingName(elem)
		return
	}

	if elem.Closing == nil {
		a.eh(epos, "Html element has no closing tag")
	}
	a.checkNonSelfClosingName(elem)
}

func (a *analyser) Visit(node astx.Node) astx.Visitor {
	switch n := node.(type) {
	case *astx.HtmlElem:
		a.analyseHtmlElem(n)
		return nil
	default:
		//nothing to do
	}
	return a
}

func (a *analyser) checkSelfClosingName(elem *astx.HtmlElem) {
	epos := a.tfile.Position(elem.Pos())
	ot := elem.Opening.Name.Name
	if !selfClosing(ot) {
		msg := fmt.Sprintf("'%v' element name doesn't exist", ot)
		a.eh(epos, msg)
	}
}

func (a *analyser) checkNonSelfClosingName(elem *astx.HtmlElem) {
	epos := a.tfile.Position(elem.Pos())
	ot := elem.Opening.Name.Name
	ct := elem.Closing.Name.Name

	if ot != ct {
		msg := fmt.Sprintf("opening and closing name of html element are not matching (%v, %v)", ot, ct)
		a.eh(epos, msg)
	}
	if !nonSelfClosing(ot) {
		msg := fmt.Sprintf("'%v' element name doesn't exist", ot)
		a.eh(epos, msg)
	}
}

func selfClosing(name string) bool {
	return contains(SelfClosingElemNames, name)
}

func nonSelfClosing(name string) bool {
	return contains(NonSelfClosingElemNames, name)
}

func contains(slice []string, str string) bool {
	for _, s := range slice {
		if s == str {
			return true
		}
	}
	return false
}

var SelfClosingElemNames = []string{
	"area",
	"base",
	"br",
	"col",
	"command",
	"embed",
	"hr",
	"img",
	"input",
	"keygen",
	"link",
	"meta",
	"param",
	"source",
	"track",
	"wbr",
}

var NonSelfClosingElemNames = []string{
	"a",
	"abbr",
	"address",
	"article",
	"aside",
	"audio",
	"b",
	"bdi",
	"bdo",
	"blockquote",
	"body",
	"button",
	"canvas",
	"caption",
	"cite",
	"code",
	"colgroup",
	"datalist",
	"dd",
	"del",
	"details",
	"dfn",
	"dialog",
	"div",
	"dl",
	"dt",
	"em",
	"fieldset",
	"figcaption",
	"figure",
	"footer",
	"form",
	"h0", "h1", "h2", "h3", "h4", "h5", "h6",
	"head",
	"header",
	"hgroup",
	"html",
	"i",
	"iframe",
	"ins",
	"kbd",
	"label",
	"legend",
	"li",
	"main",
	"map",
	"mark",
	"menu",
	"menuitem",
	"meter",
	"nav",
	"noscript",
	"object",
	"ol",
	"optgroup",
	"option",
	"output",
	"p",
	"picture",
	"pre",
	"progress",
	"q",
	"rp",
	"rt",
	"ruby",
	"s",
	"samp",
	"script",
	"section",
	"select",
	"small",
	"span",
	"strong",
	"style",
	"sub",
	"summary",
	"sup",
	"table",
	"tbody",
	"td",
	"template",
	"textarea",
	"tfoot",
	"th",
	"thead",
	"time",
	"title",
	"tr",
	"u",
	"ul",
	"var",
	"video",
}
