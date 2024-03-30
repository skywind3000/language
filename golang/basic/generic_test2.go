package main

import (
	"fmt"
	"strings"
)

// List represents a singly-linked list that holds
// values of any type.
type List[T any] struct {
	next *List[T]
	val  T
}

func (self *List[T]) Size() int {
	count := 0
	for {
		if self == nil {
			break
		}
		self = self.next
		count++
	}
	return count
}

func (self *List[T]) String() string {
	var nodes []string
	for self != nil {
		t := fmt.Sprintf("%v", self.val)
		nodes = append(nodes, t)
		self = self.next
	}
	return strings.Join(nodes, "->")
}

func main() {
	var n1 *List[int] = &List[int]{nil, 10}
	var n2 *List[int] = &List[int]{n1, 20}
	var n3 *List[int] = &List[int]{n2, 30}
	fmt.Printf("size=%d\n", n3.Size())
	fmt.Printf("%v\n", n3)
}
