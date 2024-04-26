package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("Hello, World!")
	N := 10000000
	var sum int64 = 0
	ts := time.Now().UnixNano()
	for i := 0; i < N; i++ {
		sum += time.Now().UnixNano()
	}
	dt := time.Now().UnixNano() - ts
	ns := float64(dt) / float64(N)
	fmt.Printf("time=%d N=%d once=%0.3fns\n", dt, N, ns)
	fmt.Printf("end sum=%d\n", int(sum&0xff))
}
