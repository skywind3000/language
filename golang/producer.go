package main

var cc = make(chan int)
var finish = make(chan int)

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
	go worker()
	cc <- 20
	cc <- 30
	cc <- 0
	<- finish
}

