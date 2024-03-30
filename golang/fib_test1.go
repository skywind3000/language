package main

import "fmt"

// fibonacci is a function that returns
// a function that returns an int.
func fibonacci() func() int {
	x := 0
	y := 0
	n := 0
	return func() int {
		z := 0
		if n == 0 {
			z = 0
		} else if n <= 2 {
			z = 1
		} else {
			z = x + y
		}
		x = y
		y = z
		n++
		return z
	}
}

func main() {
	f := fibonacci()
	for i := 0; i < 10; i++ {
		fmt.Println(f())
	}
}
