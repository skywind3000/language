package main

import (
	"log"
	"time"
)

const N = 40000
const PPS = 2000000

var seed uint32 = 0x11223344

func rand2() uint32 {
	seed = seed*214013 + 2531011
	return (seed >> 16) & 0x7fff
}

func random(limit int) int {
	t1 := rand2() << 16
	t2 := rand2()
	return int((t1 | t2) % uint32(limit))
}

func benchmark() {
	ras := make([]int32, N)
	cmap := make(map[int32]int32)
	for i := 0; i < N; i++ {
		for {
			k := int32(random(0x7fffffff))
			if _, ok := cmap[k]; !ok {
				ras[i] = k
				cmap[k] = int32(i)
				break
			}
		}
	}
	log.Printf("start:\n")
	ts := time.Now().UnixMilli()
	hit := 0
	hr := int32(0)
	for i := 0; i < PPS; i++ {
		p := random(N)
		k := int32(ras[p])
		v, ok := cmap[k]
		if !ok {
			log.Fatal("not find")
			break
		}
		if v != int32(p) {
			log.Fatal("not match")
			break
		} else {
			hit++
		}
		hr += v
	}
	diff := time.Now().UnixMilli() - ts
	log.Printf("end: time=%dms hit=%v hr=%v", int(diff), hit, hr%256)
}

func main() {
	log.Println("Hello, World!")
	benchmark()
}
