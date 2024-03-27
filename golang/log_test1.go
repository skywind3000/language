package main

import (
	"log"
)

func init() {
	log.SetPrefix("TRACE: ")
	log.SetFlags(log.Ldate | log.Lmicroseconds | log.Lshortfile)
}

func main() {
	log.Println("message", 1)
	log.Fatalln("fatal message", 2)
	log.Panicln("panic message", 3)
}
