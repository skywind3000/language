package main

import (
	"fmt"
)

func main() {
	a1 := [3]int{0, 0, 0}
	a2 := a1
	a2[0] = 1
	fmt.Println(a1)
	fmt.Println(a2)
}
