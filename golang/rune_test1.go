package main

import (
	"fmt"
)

func main() {
	s := []rune("你好，世界!")
	c := s[1]
	fmt.Println(len(s))
	fmt.Printf("%T\n", c)
	fmt.Println(string(s))
	n := []int{1, 2, 3, 4}
	n = n[2:]
	n = n[4:]
	fmt.Println(n)
}
