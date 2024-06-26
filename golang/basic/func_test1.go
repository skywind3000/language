// =====================================================================
//
// func_test1.go -
//
// Created by skywind on 2024/06/26
// Last Modified: 2024/06/26 17:17:37
//
// =====================================================================
package main

var fn func(int, int) int = nil

func add(a, b int) int {
	return a + b
}

func sub(a, b int) int {
	return a - b
}

func main() {
	fn = add
	println(fn(1, 2))
	fn = sub
	println(fn(1, 2))
}
