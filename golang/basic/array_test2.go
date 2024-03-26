package main

import (
	"fmt"
)

func main() {
	var array1 [3]*string
	array2 := [3]*string{new(string), new(string), new(string)}
	*array2[0] = "Red"
	*array2[1] = "Blue"
	*array2[2] = "Green"
	array1 = array2
	fmt.Println(array1)
	for i := 0; i < len(array1); i++ {
		fmt.Println(*array1[i])
	}
}
