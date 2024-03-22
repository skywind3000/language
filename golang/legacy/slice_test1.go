package main

import "fmt"

func main() {
	a := []int {1, 2, 3}
	b := append(a, 4)
	c := append(b, 5)
	fmt.Printf("slice a cap=%d len=%d: %v\n", cap(a), len(a), a)
	fmt.Printf("slice b cap=%d len=%d: %v\n", cap(b), len(b), b)
	fmt.Printf("slice c cap=%d len=%d: %v\n", cap(c), len(c), c)
	c[1] = 100
	fmt.Printf("change c[1] to 100\n")
	fmt.Printf("slice a cap=%d len=%d: %v\n", cap(a), len(a), a)
	fmt.Printf("slice b cap=%d len=%d: %v\n", cap(b), len(b), b)
	fmt.Printf("slice c cap=%d len=%d: %v\n", cap(c), len(c), c)
}

// Output:
// slice a cap=3 len=3: [1 2 3]
// slice b cap=6 len=4: [1 2 3 4]
// slice c cap=6 len=5: [1 2 3 4 5]
// change c[1] to 100
// slice a cap=3 len=3: [1 2 3]
// slice b cap=6 len=4: [1 100 3 4]
// slice c cap=6 len=5: [1 100 3 4 5]


