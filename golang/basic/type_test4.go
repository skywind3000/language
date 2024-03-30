package main

import (
	"fmt"
)

type Number int

func (self *Number) add(x Number) {
	*self += x
}

func main() {
	var x Number = 10
	x.add(5)
	fmt.Println("x = ", x)
}
