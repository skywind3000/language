package main

import "fmt"


func displaySlice(x []int) {
	fmt.Printf("slice cap=%d len=%d: %s\n", cap(x), len(x), x)
}

func main() {
	a := []int{1, 2, 3}
	b := append(a, 4)
	c := b[:3]
	d := append(b, 5)
	displaySlice(a)
	displaySlice(b)
	displaySlice(c)
	displaySlice(d)
}

