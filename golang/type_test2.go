package main

import (
	"fmt"
)

type IP []byte

func (ip IP) String() string {
	if len(ip) != 4 {
		return "Error IP"
	}
	return fmt.Sprintf("%v.%v.%v.%v", ip[0], ip[1], ip[2], ip[3])
}

func main() {
	var ip1 IP = []byte{192, 168, 1, 1}
	fmt.Println(ip1.String())
	ip1 = []byte{192, 168, 1, 3}
	fmt.Println(ip1.String())
	var ip2 IP = ip1
	ip2[3] = 4
	fmt.Println(ip1.String())
	fmt.Println(ip2.String())
}
