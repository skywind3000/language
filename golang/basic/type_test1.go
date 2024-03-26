package main

import (
	"fmt"
)

type Duration int64

func main() {
	var dur Duration
	dur = 1000
	fmt.Println(dur)
	dur = 2000
	fmt.Println(dur)
	dur = Duration(int64(1234))
	fmt.Println(dur)
}
