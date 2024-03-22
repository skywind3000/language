package main

import (
	"fmt"
)

func main() {
	slice1 := []int{10, 20, 30, 40, 50}
	slice2 := []int{100, 200, 300, 400, 500}
	fmt.Printf("\v\n", slice1)
	slice3 := append(slice1, slice2...)
	fmt.Printf("\v\n", slice1)
	fmt.Printf("\v\n", slice3)
}
