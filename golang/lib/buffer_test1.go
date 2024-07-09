package main

import (
	"bytes"
	"fmt"
)

func main() {
	var buf bytes.Buffer
	buf.Write([]byte("Hello, World!"))
	buf.Write([]byte(" 1234\n"))
	// buf.WriteTo(os.Stdout)

	s := make([]byte, 5)
	n, e := buf.Read(s)
	fmt.Printf("%v %v %v\n", s, n, e)
	n, e = buf.Read(s)
	fmt.Printf("%v %v %v\n", s, n, e)
}
