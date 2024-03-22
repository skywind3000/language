package main

import (
	"fmt"
)

func main() {
	fmt.Println("Hello, World!")
	var slice []int
	if slice == nil {
		fmt.Println("it is nil")
		fmt.Printf("size=%d cap=%d\n", len(slice), cap(slice))
		slice = append(slice, 5)
		fmt.Printf("size=%d cap=%d\n", len(slice), cap(slice))
		fmt.Println(slice)
		slice = slice[:0]
		if slice == nil {
			fmt.Println("nil again")
		} else {
			fmt.Printf("size=%d cap=%d\n", len(slice), cap(slice))
		}
	}
	slice = nil
	fmt.Printf("size=%d\n", len(slice))
	fmt.Println(slice)
	var slice2 []int = []int{1, 2, 3}
	fmt.Println(slice2)
}
