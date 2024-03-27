package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

var wg sync.WaitGroup

func init() {
	rand.Seed(time.Now().UnixNano())
}

func main() {
	court := make(chan int)
	wg.Add(2)
	go player("Nadal", court)
	go player("Djokovic", court)
	court <- 1
	wg.Wait()
}

func player(name string, court chan int) {
	defer wg.Done()
	for {
		ball, ok := <-court
		if !ok {
			println("Player", name, "won")
			return
		}
		n := rand.Intn(100)
		if n%13 == 0 {
			fmt.Printf("Player %s missed\n", name)
			close(court)
			return
		}
		fmt.Printf("Player %s hit %d\n", name, ball)
		ball++
		court <- ball
	}
}
