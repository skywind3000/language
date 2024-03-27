package main

import (
	"log"
	"sync"
	"time"
)

// ---------------------------------------------------------------------
// Worker
// ---------------------------------------------------------------------
type Worker interface {
	Task()
}

type Pool struct {
	work chan Worker
	wg   sync.WaitGroup
}

func NewPool(maxGoroutines int) *Pool {
	p := Pool{
		work: make(chan Worker),
	}

	p.wg.Add(maxGoroutines)

	for i := 0; i < maxGoroutines; i++ {
		go func() {
			for w := range p.work {
				w.Task()
			}
			p.wg.Done()
		}()
	}

	return &p
}

func (p *Pool) Run(w Worker) {
	p.work <- w
}

func (p *Pool) Shutdown() {
	close(p.work)
	p.wg.Wait()
}

// ---------------------------------------------------------------------
// main
// ---------------------------------------------------------------------
var names = []string{
	"steve",
	"bob",
	"mary",
	"therese",
	"jason",
}

type namePrinter struct {
	name string
}

func (m *namePrinter) Task() {
	log.Printf("Name: %s", m.name)
	time.Sleep(time.Second)
}

func main() {
	p := NewPool(2)
	const N = 10

	var wg sync.WaitGroup
	wg.Add(N * len(names))

	for i := 0; i < N; i++ {
		for _, name := range names {
			np := namePrinter{
				name: name,
			}

			go func() {
				p.Run(&np)
				wg.Done()
			}()
		}
	}

	wg.Wait()

	p.Shutdown()
}
