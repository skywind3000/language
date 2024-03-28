package main

import (
	"fmt"

	"github.com/skywind3000/gosub"
)

func main() {
	fmt.Println("Hello, World!")
	x := gosub.DummySub(1, 2)
	fmt.Println("1 + 2 =", x)
}
