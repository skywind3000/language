package main

import (
	"fmt"
)

func main() {
	slice := [][]int{{10}, {100, 200}}
	slice[0] = append(slice[0], 20)
	fmt.Printf("%v", slice)
}
