package main

import (
	"fmt"
	"net"
	"time"
)

func main() {
	conn, err := net.DialUDP("udp", nil, &net.UDPAddr{Port: 12345})
	if err != nil {
		fmt.Println(err)
		return
	}
	go func() {
		buf := make([]byte, 65536)
		fmt.Println("Reading from UDP")
		n, err := conn.Read(buf)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Received:", string(buf[:n]))
		}
		fmt.Println("Closing connection")
	}()
	time.Sleep(1 * time.Second)
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	fmt.Println("Deadline set")
	time.Sleep(1000 * time.Second)
}
