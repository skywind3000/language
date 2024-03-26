package main

import (
	"fmt"
)

func foo(a [5]int) {
	a[0] = 100
	fmt.Println(a)
}

func bar(a *[5]int) {
	a[0] = 200
	fmt.Println(a)
}

func main() {
	b := [...]int{1, 2, 3, 4, 5}
	foo(b)
	fmt.Println(b)
	bar(&b)
	fmt.Println(b)
}
