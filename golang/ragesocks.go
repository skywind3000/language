package main

import (
	"flag"
	"log"
	"crypto/rc4"
	"strconv"
	"net"
	"io"
	// "os"
)


//---------------------------------------------------------------------
// config
//---------------------------------------------------------------------
type Config struct {
	run_mode string
	server_host string
	client_host string
	server_port int
	client_port int
	key string
	method string
}

var config = Config {}


//---------------------------------------------------------------------
// Protocol
//---------------------------------------------------------------------
type Protocol struct {
	crypt_send *rc4.Cipher
	crypt_recv *rc4.Cipher
	twice_send *rc4.Cipher
	twice_recv *rc4.Cipher
	conn *net.TCPConn
	esock *net.TCPConn
	remote_address string
}


//---------------------------------------------------------------------
// arguments_init()
//---------------------------------------------------------------------
func arguments_init() bool {
	server := flag.Bool("server", false, "Running mode")
	server_host := flag.String("s", "", "Server address")
	client_host := flag.String("b", "", "Local address")
	server_port := flag.Int("p", 1030, "Server port")
	client_port := flag.Int("l", 1030, "Client port")
	method := flag.String("m", "rc4", "Encryption method")
	key := flag.String("k", "", "Key string")
	flag.Parse()
	config.run_mode = "client"
	if *server {
		config.run_mode = "server"
	}
	config.server_host = *server_host
	config.client_host = *client_host
	config.server_port = *server_port
	config.client_port = *client_port
	config.method = *method
	config.key = *key
	if config.run_mode == "server" {
		if config.server_host == "" {
			config.server_host = "0.0.0.0"
		}
	}	else {
		if config.server_host == "" {
			log.Print("ERROR: empty server address, use -h to help")
			return false
		}
		if config.client_host == "" {
			config.client_host = "localhost"
		}
	}
	log.Print("[config] running mode: ", config.run_mode)	
	log.Print("[config] server address: ", config.server_host)
	log.Print("[config] server port: ", config.server_port)
	log.Print("[config] client address: ", config.client_host)
	log.Print("[config] client port: ", config.client_port)
	return true
}


//---------------------------------------------------------------------
// handle error
//---------------------------------------------------------------------
func handle_error(err error) bool {
	if err == nil {
		return true
	}
	log.Printf("ERROR: %s", err.Error());
	return false
}

//---------------------------------------------------------------------
// main()
//---------------------------------------------------------------------
func main() {
	if !arguments_init() {
		return
	}

	endpoint := ""

	if config.run_mode == "server" {
		endpoint = config.server_host + ":" + strconv.Itoa(config.server_port)
	}	else {
		endpoint = config.client_host + ":" + strconv.Itoa(config.client_port)
	}

	addr, err := net.ResolveTCPAddr("tcp4", endpoint)
	if !handle_error(err) {
		return
	}

	listener, err := net.ListenTCP("tcp4", addr)
	if !handle_error(err) {
		return
	}

	log.Printf("%s is listening on %s", config.run_mode, endpoint)

	for {
		conn, err := listener.AcceptTCP()
		if err == nil {
			key := []byte(config.key)
			protocol := Protocol {}
			protocol.crypt_recv, _ = rc4.NewCipher(key)
			protocol.crypt_send, _ = rc4.NewCipher(key)
			protocol.twice_recv = nil
			protocol.twice_send = nil
			protocol.conn = conn
			protocol.esock = nil
			protocol.remote_address = ""
			if config.run_mode == "server" {
				protocol.esock = conn
				go func (protocol *Protocol) {
					defer protocol.conn.Close()
					handle_server(protocol)
				}(&protocol)
			}	else {
				go func (protocol *Protocol) {
					defer protocol.conn.Close()
					handle_client(protocol)
				}(&protocol)
			}
		}	else {
			handle_error(err)
		}
	}
}


//---------------------------------------------------------------------
// encryption send
//---------------------------------------------------------------------
func encrypt_send(protocol *Protocol, buf []byte) (int, error) {
	if (protocol.crypt_send != nil) {
		protocol.crypt_send.XORKeyStream(buf, buf)
	}
	if (protocol.twice_send != nil) {
		protocol.twice_send.XORKeyStream(buf, buf)
	}
	return protocol.esock.Write(buf)
}


//---------------------------------------------------------------------
// encryption recv
//---------------------------------------------------------------------
func encrypt_recv(protocol *Protocol, buf []byte) (int, error) {
	n, err := protocol.esock.Read(buf)
	if err != nil {
		return n, err
	}
	if (protocol.twice_recv != nil) {
		protocol.twice_recv.XORKeyStream(buf[:n], buf[:n])
	}
	if (protocol.crypt_recv != nil) {
		protocol.crypt_recv.XORKeyStream(buf[:n], buf[:n])
	}
	return n, nil
}


//---------------------------------------------------------------------
// encryption recv all
//---------------------------------------------------------------------
func encrypt_recv_all(protocol *Protocol, buf []byte) (int, error) {
	n, err := io.ReadFull(protocol.esock, buf)
	if err != nil {
		return n, err
	}
	if (protocol.twice_recv != nil) {
		protocol.twice_recv.XORKeyStream(buf, buf)
	}
	if (protocol.crypt_recv != nil) {
		protocol.crypt_recv.XORKeyStream(buf, buf)
	}
	return n, err
}


//---------------------------------------------------------------------
// server entry
//---------------------------------------------------------------------
func handle_server(protocol *Protocol) {
}


//---------------------------------------------------------------------
// client entry
//---------------------------------------------------------------------
func handle_client(protocol *Protocol) {
}



