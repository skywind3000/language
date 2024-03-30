package main

import (
	"io"
	"os"
	"strings"
)

type rot13Reader struct {
	r io.Reader
}

func (self *rot13Reader) Read(b []byte) (int, error) {
	size, err := self.r.Read(b)
	for i := 0; i < size; i++ {
		ch := int(b[i])
		if ch >= int('A') && ch <= int('Z') {
			n := ch - int('A')
			if n >= 13 {
				n = n - 13
			} else {
				n = n + 13
			}
			ch = int('A') + n
		} else if ch >= int('a') && ch <= int('z') {
			n := ch - int('a')
			if n >= 13 {
				n = n - 13
			} else {
				n = n + 13
			}
			ch = int('a') + n
		}
		b[i] = byte(ch & 0xff)
	}
	return size, err
}

func main() {
	s := strings.NewReader("Lbh penpxrq gur pbqr!")
	r := rot13Reader{s}
	io.Copy(os.Stdout, &r)
}
