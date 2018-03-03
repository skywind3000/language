//=====================================================================
//
// foolsocks.go - Simple SS Alternative
//
// Created by skywind on 2018/03/01
// Last Modified: 2018/03/02 23:49
//
//=====================================================================
// vim: set ts=4 sw=4 tw=0 noet fdm=indent foldlevel=99 :

package main

import (
	"flag"
	"log"
	"crypto/rc4"
	"strconv"
	"net"
	"io"
	"bytes"
	"encoding/gob"
	"errors"
	"math/rand"
	"time"
	"crypto/sha1"
	"fmt"
	"strings"
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
	client_host := flag.String("b", "", "Client address")
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
			log.Printf("new connection: %s", conn.RemoteAddr().String())
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
// random
//---------------------------------------------------------------------
func random_string (l int) string {  
	var result bytes.Buffer  
	var temp string  
	randInt := func (min int, max int) int {  
        rand.Seed( time.Now().UTC().UnixNano())  
        return min + rand.Intn(max-min)  
	}  
	for i:=0 ; i<l ;  {  
		if string(randInt(65,90))!=temp {  
			temp = string(randInt(65,90))  
			result.WriteString(temp)  
			i++  
		}  
	}  
	return result.String()  
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
// gob - send
//---------------------------------------------------------------------
func encrypt_send_gob(protocol *Protocol, m map[string]string) (int, error) {
	var bb bytes.Buffer
	enc := gob.NewEncoder(&bb)
	enc.Encode(m)
	buf := bb.Bytes()
	size := len(buf)
	if size >= 32767 {
		return 0, errors.New("size exceed 0x7fff")
	}
	header := [2]byte {}
	header[0] = byte(size & 0xff);
	header[1] = byte(size / 256);

	n, err := encrypt_send(protocol, header[:2])
	if err != nil {
		return n, err
	}

	if n != 2 {
		return 0, errors.New("can not send enough data")
	}

	n, err = encrypt_send(protocol, buf[:size])

	return n, err
}


//---------------------------------------------------------------------
// gob - receive
//---------------------------------------------------------------------
func encrypt_recv_gob(protocol *Protocol) (map[string]string, error) {
	conn := protocol.esock
	conn.SetReadDeadline(time.Now().Add(time.Second * 120))

	header := [2]byte {}
	n, err := encrypt_recv_all(protocol, header[:2])
	if err != nil {
		return nil, err
	}

	if n != 2 {
		return nil, errors.New("encrypt_recv_gob: size error")
	}

	var size int
	size = int(header[1]) * 256 + int(header[0])

	if size < 1 {
		return nil, errors.New("encrypt_recv_gob: invalid header size")
	}

	buf := make([]byte, size)

	n, err = encrypt_recv_all(protocol, buf[:size])

	if err != nil {
		return nil, err
	}

	if n != size {
		return nil, errors.New("encrypt_recv_gob: invalid body size")
	}

	conn.SetReadDeadline(time.Time{})

	dec := gob.NewDecoder(bytes.NewBuffer(buf))
	var m map[string]string

	err = dec.Decode(&m)

	if err != nil {
		return nil, err
	}

	return m, nil
}


//---------------------------------------------------------------------
// socks5 hand shake
//---------------------------------------------------------------------
func socks5_handshake(conn *net.TCPConn) string {
	buf := [512]byte {}
	conn.SetReadDeadline(time.Now().Add(time.Second * 25))
	n, err := io.ReadFull(conn, buf[:2])
	if !handle_error(err) {
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

	if !handle_error(err) {
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
		if !handle_error(err) {
			return ""
		}
		if n != 6 {
			log.Print("ERROR: expect more bytes")
			return ""
		}
		host = net.IPv4(buf[0],buf[1],buf[2],buf[3]).String()
	case 3:
		n, err = io.ReadFull(conn, buf[:1])
		if !handle_error(err) {
			return ""
		}
		if n != 1 {
			log.Printf("error size")
			return ""
		}
		size := int(buf[0])
		n, err = io.ReadFull(conn, buf[:size + 2])
		if !handle_error(err) {
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
		if !handle_error(err) {
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

	if host == "" {
		log.Print("ERROR: empty host")
		return ""
	}

	conn.SetReadDeadline(time.Time{})
	port := strconv.Itoa(int(buf[n-2]) * 256 | int(buf[n-1]))

	return host + ":" + port
}


//---------------------------------------------------------------------
// socks5 - establish
//---------------------------------------------------------------------
func socks5_estab(conn *net.TCPConn) {
	conn.Write([]byte{0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00})
}


//---------------------------------------------------------------------
// bidirection copy
//---------------------------------------------------------------------
func encrypt_copy(protocol *Protocol) {
	const BUF_SIZE int = 32768
	ch := make(chan int)
	go func () {
		buf := make([]byte, BUF_SIZE)
		for {
			n, err := encrypt_recv(protocol, buf[:BUF_SIZE])
			if err == nil && n > 0 {
				n, err = protocol.conn.Write(buf[:n])
				if err != nil {
					break
				}
			}	else {
				if err == nil {
					log.Printf("ERROR: Fatal recv zero")
				}
				break
			}
		}
		ch <- 1
	}()
	go func () {
		buf := make([]byte, BUF_SIZE)
		for {
			n, err := protocol.conn.Read(buf)
			if err == nil && n > 0 {
				n, err = encrypt_send(protocol, buf[:n])
				if err != nil {
					break
				}
			}	else {
				if err == nil {
					log.Printf("ERROR: Fatal recv zero")
				}
				break
			}
		}
		ch <- 2
	}()
	<- ch
	<- ch
}


//---------------------------------------------------------------------
// client entry
//---------------------------------------------------------------------
func handle_client(protocol *Protocol) {
	conn := protocol.conn

	address := socks5_handshake(conn)

	if address == "" {
		return
	}

	log.Printf("destination: %s", address)

	endpoint := config.server_host + ":" + strconv.Itoa(config.server_port)
	remote, err := net.ResolveTCPAddr("tcp4", endpoint)

	if !handle_error(err) {
		return
	}

	client, err := net.DialTCP("tcp4", nil, remote)
	if !handle_error(err) {
		return
	}

	protocol.esock = client
	defer client.Close()

	m := map[string]string {}
	m["random"] = random_string(12)
	m["destination"] = address
	m["name"] = "foolsocks"
	m["secret"] = random_string(10)
	m["signature"] = signature_sign(config.key)

	_, err = encrypt_send_gob(protocol, m)

	if !handle_error(err) {
		return
	}

	r, err := encrypt_recv_gob(protocol)

	if !handle_error(err) {
		return
	}

	if r["status"] != "ok" {
		log.Printf("status is not ok")
		return
	}

	socks5_estab(protocol.conn)

	secret := m["secret"] + r["secret"]
	rc4key := []byte(secret)
	protocol.twice_recv, _ = rc4.NewCipher(rc4key)
	protocol.twice_send, _ = rc4.NewCipher(rc4key)

	log.Printf("established from %s to %s", 
		protocol.conn.RemoteAddr().String(), address)

	encrypt_copy(protocol)
	log.Printf("end transport")
}


//---------------------------------------------------------------------
// server entry
//---------------------------------------------------------------------
func handle_server(protocol *Protocol) {
	// conn := protocol.conn
	m, err := encrypt_recv_gob(protocol)
	if !handle_error(err) {
		return
	}

	address := m["destination"]

	if address == "" {
		log.Printf("handle_server: empty destination")
		return
	}

	signature := m["signature"]

	if !signature_check(config.key, signature) {
		log.Printf("signature check failed")
		return
	}

	remote, err := net.ResolveTCPAddr("tcp4", address)
	if !handle_error(err) {
		return
	}

	log.Printf("destination=%s", m["destination"])

	client, err := net.DialTCP("tcp4", nil, remote)
	if !handle_error(err) {
		return
	}

	protocol.conn = client
	defer client.Close()

	r := map[string]string {}
	r["name"] = "server"
	r["remote"] = client.RemoteAddr().String()
	r["status"] = "ok"
	r["secret"] = random_string(10)

	_, err = encrypt_send_gob(protocol, r)

	if !handle_error(err) {
		return
	}

	log.Printf("established from %s to %s", 
		protocol.esock.RemoteAddr().String(), address)

	secret := m["secret"] + r["secret"]
	rc4key := []byte(secret)
	protocol.twice_recv, _ = rc4.NewCipher(rc4key)
	protocol.twice_send, _ = rc4.NewCipher(rc4key)

	encrypt_copy(protocol)
}


//---------------------------------------------------------------------
// md5_sign
//---------------------------------------------------------------------
func md5_sign(ts int64, key string) string {
	token := strconv.FormatInt(ts, 10) + ":" + key
	sum := sha1.Sum([]byte(token))
	str := fmt.Sprintf("%x", sum)
	str = strconv.FormatInt(ts, 10) + "i" + str
	return str
}


//---------------------------------------------------------------------
// md5_check
//---------------------------------------------------------------------
func md5_check(ts int64, key string, sign string, diff int64) bool {
	pos := strings.Index(sign, "i")
	if pos < 0 {
		return false
	}
	timestamp := sign[:pos]
	tt, err := strconv.ParseInt(timestamp, 10, 64)
	if err != nil {
		return false
	}
	delta := ts - tt
	if delta < 0 {
		delta = -delta
	}
	if delta > diff {
		return false
	}
	test := md5_sign(tt, key)
	if test != sign {
		return false
	}
	return true
}


//---------------------------------------------------------------------
// make md5 signature from current timestamp and key
//---------------------------------------------------------------------
func signature_sign(key string) string {
	ts := time.Now().Unix()
	return md5_sign(ts, key)
}


//---------------------------------------------------------------------
// check signature with current timestamp and key
//---------------------------------------------------------------------
func signature_check(key string, sign string) bool {
	ts := time.Now().Unix()
	return md5_check(ts, key, sign, 600)
}


