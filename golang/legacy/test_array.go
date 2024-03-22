package main

func main() {
	b := [10]byte {0, 0, 0, 0}
	d := b[:2]
	b[0] = 10
	d[0] = 20
	println("b[0] is", b[0])
	println("d[0] is", d[0])
}


