package main

import "fmt"

func main() {
	// var m map[string]int = map[string]int{}
	var m map[string]int = make(map[string]int)
	// m = make(map[string]int)
	if m == nil {
		println("m is nil")
	}
	m["one"] = 1
	fmt.Println(m)
	fmt.Println(m["one"])
	fmt.Println(len(m))
	value, exists := m["two"]
	println(value, exists)
	value, exists = m["one"]
	println(value, exists)
}
