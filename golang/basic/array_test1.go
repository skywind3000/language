package main

import "fmt"

func main() {
	a := [...]int{1, 2, 3}
	b := a[1:2]
	c := append(b, 9)
	b[0] = 100
	var d [5]int = [5]int{1: 100, 3: 200}
	fmt.Println(a)
	fmt.Println(cap(a))
	fmt.Println(len(b))
	fmt.Println(b)
	fmt.Println(c)
	fmt.Println(d)
}
