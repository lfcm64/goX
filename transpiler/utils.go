package transpiler

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"os"
)

func readSource(filename string, src any) ([]byte, error) {
	if src != nil {
		switch s := src.(type) {
		case string:
			return []byte(s), nil
		case []byte:
			return s, nil
		case *bytes.Buffer:
			// is io.Reader, but src is already available in []byte form
			if s != nil {
				return s.Bytes(), nil
			}
		case io.Reader:
			return io.ReadAll(s)
		}
		return nil, errors.New("invalid source")
	}
	return os.ReadFile(filename)
}

const defaultCompiler = "source"

var compiler = flag.String("c", defaultCompiler, "compiler used for installed packages (gc, gccgo, or source)")

func (t *Transpiler) report0(err error) {
	type bailout struct{}
	fmt.Println(err)

	switch et := err.(type) {
	case scanner.ErrorList:
		for _, e := range et {
			t.errors.Add(e)
		}
	default:
		t.errors.Add(et)
	}

	if t.errors.Lenght() >= 10 {
		panic(bailout{})
	}
}

func (t *Transpiler) report(pos token.Position, msg string) {
	err := &scanner.Error{Pos: pos, Msg: msg}
	t.report0(err)
}
