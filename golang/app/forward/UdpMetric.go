// =====================================================================
//
// UdpMetric.go -
//
// Last Modified: 2024/07/15 11:00:04
//
// =====================================================================
package forward

import (
	"fmt"
	"sync/atomic"
)

type UdpMetric struct {
	PacketReceived atomic.Int64 // packet received
	PacketSent     atomic.Int64 // packet sent
	PacketDropped  atomic.Int64 // error received
}

func (self *UdpMetric) Clear() {
	self.PacketReceived.Store(0)
	self.PacketSent.Store(0)
	self.PacketDropped.Store(0)
}

func (self *UdpMetric) IncPacketReceived() {
	self.PacketReceived.Add(1)
}

func (self *UdpMetric) IncPacketSent() {
	self.PacketSent.Add(1)
}

func (self *UdpMetric) IncPacketDropped() {
	self.PacketDropped.Add(1)
}

func (self *UdpMetric) String() string {
	t := fmt.Sprintf("UdpMetric(recv=%d, sent=%d, drop=%d", self.PacketReceived.Load(), self.PacketSent.Load(), self.PacketDropped.Load())
	return t
}

func (self *UdpMetric) Clone() *UdpMetric {
	t := &UdpMetric{
		PacketReceived: atomic.Int64{},
		PacketSent:     atomic.Int64{},
		PacketDropped:  atomic.Int64{},
	}
	t.PacketReceived.Store(self.PacketReceived.Load())
	t.PacketSent.Store(self.PacketSent.Load())
	t.PacketDropped.Store(self.PacketDropped.Load())
	return t
}
