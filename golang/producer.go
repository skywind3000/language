package main

var cc chan int
var finish chan int

func worker() {
	for {
		x := <- cc
		if x <= 0 {
			println("exit")
			break
		}	else {
			println("got", x)
		}
	}
	finish <- 10
}

func main() {
	cc = make(chan int)
	finish = make(chan int)
	go worker()
	cc <- 20
	cc <- 30
	cc <- 0
	<- finish
}

