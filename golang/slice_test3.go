//  vim: set ts=4 sw=4 tw=0 noet ft=go :
package main

import (
	"fmt"
)

func main() {
	var a [100]int
	b := make([]int, 256)
	a[0] = 1
	for i := 0; i < len(b); i++ {
		b[i] = i;
	}
	copy(a[5:], b[:10])
	for i := 0; i < 20; i++ {
		fmt.Println(a[i])
	}
	var z [3]int = [...]int{1, 2, 3}
	fmt.Println(z)
}


