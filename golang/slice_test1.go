package main

import (
	"fmt"
)

func main() {
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
	slice3 := make([]int, 0)
	if slice3 == nil {
		fmt.Println("slice3 is nil")
		fmt.Printf("size=%d cap=%d\n", len(slice3), cap(slice3))
	} else {
		fmt.Println("slice3 is not nil")
		fmt.Printf("size=%d cap=%d\n", len(slice3), cap(slice3))
	}
}
