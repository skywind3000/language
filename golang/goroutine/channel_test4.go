package main

import (
	"fmt"
	"time"
)

func scale(x int) int {
	return x * 2
}

var fn func(int) int = scale

func main() {
	fmt.Println(time.Now().UnixNano())
	x := 10
	t1 := time.Now().UnixNano()
	y := fn(x)
	t1 = time.Now().UnixNano() - t1
	fmt.Println(y, t1)
	ch1 := make(chan int)
	ch2 := make(chan int)
	t2 := time.Now().UnixNano()
	go func() {
		s := <-ch1
		t := fn(s)
		ch2 <- t
	}()
	ch1 <- x
	z := <-ch2
	t2 = time.Now().UnixNano() - t2
	fmt.Println(z, t2)
}
