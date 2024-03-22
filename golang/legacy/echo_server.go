package main

import ( 
	"net"
	// "os"
	"fmt"
	"runtime"
	"io"
)

const BUFF_SIZE = 1024

func handleConn(tcpConn *net.TCPConn) {
	if tcpConn == nil {
		return
	}

	var buff = make([]byte, BUFF_SIZE)

	for {
		n, err := tcpConn.Read(buff)
		if err == io.EOF {
			fmt.Printf("The RemoteAddr: %s is closed !\n", tcpConn.RemoteAddr().String())
			return
		}
		handleError(err)
		if string(buff[:n]) == "exit" {
			fmt.Printf("The client: %s has exited\n", tcpConn.RemoteAddr().String())
		}
		if n > 0 {
			fmt.Printf("Read: %s\n", string(buff[:n]))
			tcpConn.Write(buff[:n])
		}
	}
}

func handleError(err error) {
	if err == nil {
		return
	}
	fmt.Printf("error: %s\n", err.Error());
}

func main() {
	runtime.GOMAXPROCS(1)
	tcpAddr, err := net.ResolveTCPAddr("tcp4", "localhost:3000")
	handleError(err)
	tcpListener, err := net.ListenTCP("tcp4", tcpAddr)
	handleError(err)
	defer tcpListener.Close()
	println("Listening on port 3000")
	for {
		tcpConn, err := tcpListener.AcceptTCP()
		fmt.Printf("The client: %s has connected !\n", tcpConn.RemoteAddr().String())
		handleError(err)
		defer tcpConn.Close()
		go handleConn(tcpConn)
	}
}



