package main

import (
	"time"
	"net"
	"log"
	"io"
	"strconv"
)

type Config struct {
	server_port int
	key string
	timeout int
}

var config = Config { 3000, "", 5 }

func handleError(err error) bool {
	if err == nil {
		return true
	}
	log.Printf("ERROR: %s", err.Error());
	return false
}

func socks5_handshake(conn *net.TCPConn) string {
	buf := [512]byte {}
	conn.SetReadDeadline(time.Now().Add(time.Second * 10))
	n, err := io.ReadFull(conn, buf[:2])
	if !handleError(err) {
		return ""
	}
	if n != 2 || n <= 0 {
		log.Printf("ERROR: handshake error 1")
		return ""
	}
	if buf[0] != 5 {
		log.Printf("ERROR: unsupported protocol: %d", buf[0])
		return ""
	}
	n = int(buf[1])
	m, err := io.ReadFull(conn, buf[:n])
	if m != n {
		log.Printf("ERROR: size mismatch %d/%d", m, n)
		return ""
	}

	conn.Write([]byte{0x05, 0x00})

	n, err = io.ReadFull(conn, buf[:4])

	if !handleError(err) {
		return ""
	}

	if n !=4 {
		log.Printf("ERROR: bad header")
		return ""
	}

	if buf[0] != 5 {
		log.Printf("ERROR: unsupported protocol: %d", buf[0])
		return ""
	}

	if buf[1] != 1 {
		log.Print("ERROR: unsupported cmd: %d", buf[1])
		return ""
	}

	var host string

	switch buf[3] {
	case 1:
		n, err = io.ReadFull(conn, buf[:6])
		if !handleError(err) {
			return ""
		}
		if n != 6 {
			log.Print("ERROR: expect more bytes")
			return ""
		}
		host = net.IPv4(buf[0],buf[1],buf[2],buf[3]).String()
	case 3:
		n, err = io.ReadFull(conn, buf[:1])
		if !handleError(err) {
			return ""
		}
		if n != 1 {
			log.Printf("error size")
			return ""
		}
		size := int(buf[0])
		n, err = io.ReadFull(conn, buf[:size + 2])
		if !handleError(err) {
			return ""
		}
		if n != size + 2 {
			log.Print("ERROR: expect more bytes")
			return ""
		}
		host = string(buf[:n - 2])
		// log.Printf("host2 is %s size is %d", host, size)
	case 4:
		n, err = io.ReadFull(conn, buf[:18])
		if !handleError(err) {
			return ""
		}
		if n != 18 {
			log.Print("ERROR: expect more bytes")
			return ""
		}
		b := buf[:16]
		host = net.IP{b[4], b[5], b[6], b[7], b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15], b[16], b[17], b[18], b[19]}.String()
		// log.Printf("host3 is %s", host)
	default:
		log.Print("ERROR: unsupport address type: %d", buf[3])
		return ""
	}

	conn.SetReadDeadline(time.Time{})
	port := strconv.Itoa(int(buf[n-2]) * 256 | int(buf[n-1]))

	return host + ":" + port
}

func socks5_handle(conn *net.TCPConn) {
	log.Printf("new connection: %s", conn.RemoteAddr().String())
	defer conn.Close()

	address := socks5_handshake(conn)

	if address == "" {
		return
	}

	log.Printf("destination: %s", address)

	client, err := net.Dial("tcp", address)
	if !handleError(err) {
		return
	}

	defer client.Close()
	conn.Write([]byte{0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00})

	log.Printf("begin transfer")

	go io.Copy(conn, client)
	io.Copy(client, conn)

	log.Print("finish")
}

func socks5_listen(address string) {
	tcpAddr, err := net.ResolveTCPAddr("tcp4", address)
	if !handleError(err) {
		return
	}
	tcpListener, err := net.ListenTCP("tcp4", tcpAddr)
	if !handleError(err) {
		return
	}
	defer tcpListener.Close()
	log.Printf("listening on %s", address)
	for {
		tcpConn, err := tcpListener.AcceptTCP()
		if err == nil {
			go socks5_handle(tcpConn)
		}	else {
			handleError(err)
		}
	}
}


func main() {
	socks5_listen("localhost:1030")
}


