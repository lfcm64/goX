package transformer

import (
	"goX/transpiler/astx"
	"strings"
)

var chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

type namer struct {
	names map[string]bool
}

func (n *namer) findName(file *astx.File) string {
	n.names = map[string]bool{}

	for _, decl := range file.Decls {
		astx.Walk(n, decl)
	}

	b := strings.Builder{}

	if n.testName("x") {
		return "x"
	}
	b.WriteByte('x')

	for {
		for i, ch := range chars {
			name := b.String() + string(ch)
			if n.testName(name) {
				return name
			}
			if i == 61 {
				b.WriteRune(ch)
			}
		}
	}
}

func (n *namer) testName(name string) bool {
	for item := range n.names {
		if strings.HasPrefix(item, name) && len(item) > len(name) {
			return false
		}
	}
	return true
}

func (n *namer) Visit(node astx.Node) astx.Visitor {
	switch x := node.(type) {
	case *astx.Ident:
		if x.Name[0] == 'x' {
			n.names[x.Name] = true
		}
	default:
		//nothing to do
	}
	return n
}
