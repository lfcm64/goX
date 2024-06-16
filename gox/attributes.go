package gox

import "fmt"

type Attr struct {
	Key   string
	Value string
}

func (a *Attr) String() string {
	return fmt.Sprintf("%s=\"%s\" ", a.Key, a.Value)
}
