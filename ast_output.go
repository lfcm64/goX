package main

import "goX/gox"

func hello(name string) gox.Component {
	return func() gox.Element {
		x0 := gox.E("h1")
		x0.T("Hello", name, "!")
		return x0
	}
}
