package main

import "fmt"

func Index[T comparable](s []T, t T) int {
	for i, v := range s {
		if v == t {
			return i
		}
	}
	return -1
}

func main() {
	si := []int{10, 20, 15, -10}
	fmt.Println(Index(si, 15)) // 2
	ss := []string{"foo", "bar", "baz"}
	fmt.Println(Index(ss, "hello")) // -1
}
