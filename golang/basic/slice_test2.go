package main

import (
	"fmt"
)

func printSlice(x []int) {
	fmt.Printf("slice[%d/%d]: ", len(x), cap(x))
	for i := 0; i < len(x); i++ {
		fmt.Printf("%d ", x[i])
	}
	fmt.Println("")
}

func main() {
	fmt.Println("Hello, World!")
	a := []int{0}
	a = append(a, 1, 2, 3)
	printSlice(a)
	println(cap(a))
	println(len(a))
	a[2] = 200
	printSlice(a)
	b := a[1:3]
	printSlice(b)
	c := b[0:3]
	printSlice(c)
}
