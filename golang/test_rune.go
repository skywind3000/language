package main

import (
    "fmt"
    "reflect"
)

func main() {
    // Creating runes
    rune1 := 'B'
    rune2 := 'g'
    rune3 := '\a'

    // Displaying rune and its type
    fmt.Printf("Rune 1: %c; Unicode: %U; Type: %s\n", rune1, rune1, reflect.TypeOf(rune1))
    fmt.Printf("Rune 2: %c; Unicode: %U; Type: %s\n", rune2, rune2, reflect.TypeOf(rune2))
    fmt.Printf("Rune 3: Unicode: %U; Type: %s\n", rune3, reflect.TypeOf(rune3))
	fmt.Printf("Hello\n");
}


