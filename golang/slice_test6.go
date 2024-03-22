package main

import "fmt"

func main() {
	slice := []int{10, 20, 30, 40, 50}
	for index, value := range slice {
		fmt.Printf("Value: %d, Value-Addr: %X, ElemAddr: %X\n", value, &value, &slice[index])
	}
}
