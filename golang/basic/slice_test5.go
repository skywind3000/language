package main

import (
	"fmt"
)

func printSlice(slice []int) {
	fmt.Printf("slice[%d/%d]: ", len(slice), cap(slice))
	for _, v := range slice {
		fmt.Printf("%d ", v)
	}
	fmt.Println()
}

func main() {
	slice := []int{10, 20, 30, 40, 50}
	printSlice(slice)
	newSlice := slice[1:2:3]
	s1 := append(newSlice, 60)
	s2 := append(s1, 70)
	println()
	printSlice(slice)
	printSlice(newSlice)
	printSlice(s1)
	printSlice(s2)
	fmt.Printf("slice: %v\n", slice)
}
