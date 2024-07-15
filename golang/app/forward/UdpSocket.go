// =====================================================================
//
// # UdpSocket.go - UdpSocket interface
//
// Last Modified: 2024/07/15 10:10:39
//
// =====================================================================
package forward

import (
	"log"
	"net"
)

const (
	UDP_RECV_COUNT = 3
)

// UdpSocket interface
type UdpSocket interface {

	// open an udp socket and bind to the address
	Open(addr *net.UDPAddr, flags int) error

	// close the udp socket
	Close()

	// set option
	SetOption(option int, value interface{}) error

	// set the on packet callback
	SetCallback(receiver func(data []byte, addr *net.UDPAddr) bool)

	// send data to the udp socket
	SendTo(data []byte, addr *net.UDPAddr) int

	// send data to the udp socket
	SendBatch(data [][]byte, addr []*net.UDPAddr) int

	// get the udp metric
	GetMetric() *UdpMetric

	// set the packet logger
	SetLogPacket(logger *log.Logger)

	// set the error logger
	SetLogError(logger *log.Logger)
}
