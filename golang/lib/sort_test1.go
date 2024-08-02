package main

import (
	"fmt"
	"sort"
)

func main() {
	array := []int{10, 5, 3, 7, 2, 8, 9, 4, 6, 1}
	fmt.Println(array)
	sort.Ints(array)
	fmt.Println(array)
}
