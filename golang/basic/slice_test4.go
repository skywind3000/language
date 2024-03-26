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
	newSlice := slice[1:3]
	printSlice(slice)
	printSlice(newSlice)
	newSlice2 := append(newSlice, 60)
	printSlice(slice)
	printSlice(newSlice)
	printSlice(newSlice2)
}
