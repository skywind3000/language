// =====================================================================
//
// UdpSocket_Basic.go - UdpSocket implementation
//
// Last Modified: 2024/07/15 10:25:24
//
// =====================================================================
package forward

import (
	"log"
	"net"
	"sync"
)

// basic udp socket implementation
type UdpSocket_Basic struct {
	receiver  func(data []byte, addr *net.UDPAddr) error
	metric    UdpMetric
	conn      *net.UDPConn
	closing   bool
	wg        sync.WaitGroup
	count     int
	logPacket *log.Logger
	logError  *log.Logger
}

func NewUdpSocket_Basic() *UdpSocket_Basic {
	self := &UdpSocket_Basic{
		receiver:  nil,
		conn:      nil,
		closing:   false,
		count:     4,
		logPacket: nil,
		logError:  nil,
	}
	self.metric.Clear()
	return self
}

func (self *UdpSocket_Basic) SetCallback(receiver func(data []byte, addr *net.UDPAddr) error) {
	self.receiver = receiver
}

func (self *UdpSocket_Basic) Open(addr *net.UDPAddr, flags int) error {
	self.Close()
	self.metric.Clear()
	err := error(nil)
	self.conn, err = net.ListenUDP("udp", addr)
	if err != nil {
		return err
	}
	self.closing = false
	self.wg.Add(self.count)
	for i := 0; i < self.count; i++ {
		go func() {
			defer self.wg.Done()
			data := make([]byte, 1024*64)
			for !self.closing {
				n, addr, err := self.conn.ReadFromUDP(data)
				if err != nil {
					break
				} else {
					if self.logPacket != nil {
						self.logPacket.Printf("ReadFromUDP: size=%d addr=%s", n, addr.String())
					}
				}
				if n > 0 && self.receiver != nil {
					err := self.receiver(data[:n], addr)
					if err != nil {
						self.metric.IncPacketDropped()
					} else {
						self.metric.IncPacketReceived()
					}
				}
			}
		}()
	}
	return nil
}

func (self *UdpSocket_Basic) Close() {
	self.closing = true
	if self.conn != nil {
		self.conn.Close()
	}
	self.wg.Wait()
	if self.conn != nil {
		self.conn.Close()
		self.conn = nil
	}
}

func (self *UdpSocket_Basic) SendTo(data []byte, addr *net.UDPAddr) int {
	if self.conn != nil {
		n, err := self.conn.WriteToUDP(data, addr)
		if err != nil {
			if self.logError != nil {
				self.logError.Printf("SendTo: %s", err)
			}
			return -1
		} else {
			if self.logPacket != nil {
				self.logPacket.Printf("SendTo: size=%d addr=%s", len(data), addr.String())
			}
		}
		if n > 0 {
			self.metric.IncPacketSent()
		}
	}
	return 0
}

func (self *UdpSocket_Basic) SendBatch(data [][]byte, addr []*net.UDPAddr) int {
	for i := 0; i < len(data); i++ {
		self.SendTo(data[i], addr[i])
	}
	return 0
}

func (self *UdpSocket_Basic) GetMetric() *UdpMetric {
	return &self.metric
}

func (self *UdpSocket_Basic) SetLogPacket(logger *log.Logger) {
	self.logPacket = logger
}

func (self *UdpSocket_Basic) SetLogError(logger *log.Logger) {
	self.logError = logger
}

func (self *UdpSocket_Basic) SetOption(option int, value interface{}) error {
	var err error = nil
	switch option {
	case UDP_RECV_COUNT:
		self.count = value.(int)
	}
	return err
}
