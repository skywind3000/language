package main

import "fmt"

func main() {
	ch1 := make(chan int, 100)
	ch1 <- 10
	fmt.Printf("len(ch1)=%d, cap(ch1)=%d\n", len(ch1), cap(ch1))
}
