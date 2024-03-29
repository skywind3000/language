package main

import (
	"log"
	"math/rand"
	"time"
)

const N = 40000
const PPS = 1000000

func benchmark() {
	ras := make([]int32, N)
	cmap := make(map[int32]int32)
	for i := 0; i < N; i++ {
		for {
			k := int32(rand.Intn(0x7fffffff))
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
	for i := 0; i < PPS; i++ {
		p := rand.Intn(N)
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
	}
	diff := time.Now().UnixMilli() - ts
	log.Printf("end: time=%dms hit=%v", int(diff), hit)
}

func main() {
	log.Println("Hello, World!")
	benchmark()
}
