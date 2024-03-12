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

func displaySlice(x []int) {
	fmt.Printf("slice cap=%d len=%d: %s\n", cap(x), len(x), x)
}

func main() {
	x := [...]int{1, 2, 3};
	y := x[0:2];
	z := []int{1, 2, 3};
	fmt.Printf("%s\n", isSliceOrArray(x))
	fmt.Printf("%s\n", isSliceOrArray(y))
	fmt.Printf("%s\n", isSliceOrArray(z))
	fmt.Printf("cap=%d size=%d\n", cap(z), len(z))
	u := append(z, 4)
	v := append(u, 5)
	v[1] = 100
	displaySlice(z)
	displaySlice(u)
	displaySlice(v)
}


