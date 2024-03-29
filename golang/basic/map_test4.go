package main

import (
	"fmt"
)

func foo(m map[string]string) {
	m["foo"] = "bar"
}

func main() {
	m := make(map[string]string)
	m["hello"] = "world"
	foo(m)
	fmt.Println(m)
}
