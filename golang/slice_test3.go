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
	a := []int{1, 2, 3, 4, 5}
	b := a[2:4]
	printSlice(a)
	printSlice(b)
	b = append(b, 9)
	printSlice(a)
	printSlice(b)
}
