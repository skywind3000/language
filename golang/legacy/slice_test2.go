package main

import "fmt"


func displaySlice(x []int) {
	fmt.Printf("slice cap=%d len=%d: %s\n", cap(x), len(x), x)
}

func main() {
	a := []int{1, 2, 3}
	b := append(a, 4)
	c := b[:3]
	d := append(c, 5)
	displaySlice(a)
	displaySlice(b)
	displaySlice(c)
	displaySlice(d)
}

// Output:
// slice cap=3 len=3: [%!s(int=1) %!s(int=2) %!s(int=3)]
// slice cap=6 len=4: [%!s(int=1) %!s(int=2) %!s(int=3) %!s(int=5)]
// slice cap=6 len=3: [%!s(int=1) %!s(int=2) %!s(int=3)]
// slice cap=6 len=4: [%!s(int=1) %!s(int=2) %!s(int=3) %!s(int=5)]

