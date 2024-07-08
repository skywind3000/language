package main

import "fmt"

func main() {
	s1 := []int{1, 2, 3, 4, 5}
	s2 := []int{0, 0, 0}
	copy(s2, s1)
	fmt.Println(s2)
}
