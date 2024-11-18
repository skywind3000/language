// =====================================================================
//
// func_test2.go -
//
// Created by skywind on 2024/11/18
// Last Modified: 2024/11/18 20:59:42
//
// =====================================================================
package main

type FnType func()

type Worker struct {
	name   string
	salary int
}

func NewWorker(name string, salary int) *Worker {
	return &Worker{name, salary}
}

func (self *Worker) Show() {
	println("Worker:", self.name, self.salary)
}

func main() {
	w1 := NewWorker("Tom", 1000)
	w2 := NewWorker("Jerry", 2000)
	var fn FnType = nil
	fn = w1.Show
	fn()
	fn = w2.Show
	fn()
}
