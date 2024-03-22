package main

import (
	"fmt"
	"reflect"
)


func main() {
	var x, y int = 1, 2
	fmt.Printf("%s\n", reflect.TypeOf(x))
	fmt.Printf("%s\n", reflect.TypeOf(y))
	z := [4]int{1,2,3};
	fmt.Printf("%s\n", z)
	var w *int = &x;
	fmt.Printf("%s\n", reflect.TypeOf(w))
}

