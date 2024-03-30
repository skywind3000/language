package main

import (
	"fmt"
)

type Number int

func (self *Number) add(x Number) {
	*self += x
}

func (self Number) mul(x Number) Number {
	return self * x
}

func main() {
	var x Number = 10
	x.add(5)
	fmt.Println("x = ", x)
	fmt.Println(Number(10).mul(5))
}
