package gox

import (
	"fmt"
)

type Component func() Element

func (c Component) String() string {
	element := c()
	return element.String()
}

type Element struct {
	Name        string
	Text        string
	SelfClosing bool
	Attrs       []Attr
	Children    []*Element
}

func E(name string) Element {
	return Element{Name: name}
}

func E1(name string) Element {
	return Element{Name: name, SelfClosing: true}
}

func (e *Element) T(content ...any) {
	for _, text := range content {
		e.Text = e.Text + fmt.Sprintf("%d", text)
	}
}

func (e *Element) A(key string, value string) {
	attr := Attr{key, value}
	e.Attrs = append(e.Attrs, attr)
}

func (e *Element) C(child Element) {
	e.Children = append(e.Children, &child)
}

func (e *Element) C1(child Component) {
	elem := child()
	e.Children = append(e.Children, &elem)
}

func (e *Element) String() string {
	var attrs string
	for _, attr := range e.Attrs {
		attrs += attr.String()
	}
	var children string

	for _, child := range e.Children {
		children += child.String()
	}

	if e.SelfClosing {
		return fmt.Sprintf("<%s %s/>", e.Name, attrs)
	}
	return fmt.Sprintf("<%s %s>%s</%s>", e.Name, attrs, children, e.Name)
}
