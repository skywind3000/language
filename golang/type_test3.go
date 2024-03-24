package main

import (
	"fmt"
	"net"
)

// net.IP definition: "type IP []byte"
func main() {
	ip1 := net.IP{192, 168, 1, 1}
	ip2 := ip1
	fmt.Println(ip1)
	fmt.Println(ip2)
	ip2[3] = 5
	fmt.Println(ip1)
	fmt.Println(ip2)
}

/*
output:

	192.168.1.1
	192.168.1.1
	192.168.1.5
	192.168.1.5
*/
