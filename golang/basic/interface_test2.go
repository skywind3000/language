package main

import (
	"fmt"
)

type duration int

func (d duration) pretty() string {
	return fmt.Sprintf("Duration: %d", d)
}

func main() {
	duration(42).pretty()
}
