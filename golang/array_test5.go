package main

import (
	"fmt"
	"reflect"
)

func isSliceOrArray(x interface{}) string {
	t := reflect.TypeOf(x)
	switch t.Kind() {
	case reflect.Slice:
		return "Slice"
	case reflect.Array:
		return "Array"
	default:
		return "not a slice or an array"
	}
}

func main() {
	a := [...]int{1, 2, 3, 4, 5}
	fmt.Println(a)
	var b []int = a[:]
	fmt.Println(b)
	fmt.Println(isSliceOrArray(a))
	fmt.Println(isSliceOrArray(b))
	c := a[1:3]
	c[1] = 5
	fmt.Println(a)
}
