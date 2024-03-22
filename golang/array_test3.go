package main

import (
	"fmt"
)

func main() {
	var array1 [4][2]int
	array2 := [4][2]int{{10, 11}, {20, 21}, {30, 31}, {40, 41}}
	array1 = array2
	fmt.Println(array1)
}
