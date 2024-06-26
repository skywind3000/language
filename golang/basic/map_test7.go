package main

import "fmt"

func main() {
	table := make(map[string]string)
	table["1"] = "A"
	table["2"] = "B"
	delete(table, "3")
	fmt.Printf("%t\n", table)
	fmt.Printf("%v\n", table)
}
