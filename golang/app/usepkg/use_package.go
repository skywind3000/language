package main

import (
	"fmt"

	"github.com/skywind3000/gosub"
	dummy2 "github.com/skywind3000/gosub/dummy"
)

func main() {
	fmt.Println("Hello, World!")
	x := gosub.DummySub(1, 2)
	fmt.Println("1 + 2 =", x)
	y := dummy2.DummyAdd2(9, 10)
	fmt.Println("add2=", y)
}
