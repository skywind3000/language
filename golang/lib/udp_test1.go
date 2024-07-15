package main

import (
	"fmt"
	"net"
	"time"
)

func main() {
	// Create a UDP listener
	ln, err := net.ListenUDP("udp", &net.UDPAddr{Port: 12345})
	if err != nil {
		fmt.Println(err)
		return
	}
	defer ln.Close()

	go func() {
		buf := make([]byte, 1024)
		n, addr, err := ln.ReadFromUDP(buf)
		if err != nil {
			fmt.Printf("Error reading from UDP: %v\n", err)
			return
		}
		fmt.Printf("Received message from %s: %s\n", addr, string(buf[:n]))
	}()

	time.Sleep(1 * time.Second) // Simulate some work

	// Close the connection after a delay
	time.AfterFunc(2*time.Second, func() {
		err = ln.Close()
		if err != nil {
			fmt.Printf("Error closing UDP connection: %v\n", err)
		}
	})

	select {} // Keep the main goroutine alive
}
