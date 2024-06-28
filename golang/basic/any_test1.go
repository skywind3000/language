package main

type Packet struct {
	cmd  int
	data int
}

func NewPacket(cmd int, data int) *Packet {
	return &Packet{cmd, data}
}

func main() {
	pending := make([]any, 0)
	for i := 0; i < 10; i++ {
		p := NewPacket(1, i)
		pending = append(pending, p)
	}
	for i := 0; i < len(pending); i++ {
		p := pending[i].(*Packet)
		println(p.cmd, p.data)
	}
}
