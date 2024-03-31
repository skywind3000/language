package main

import (
	"fmt"

	"golang.org/x/tour/tree"
)

/*
type Tree struct {
    Left  *Tree
    Value int
    Right *Tree
}
*/

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	if t.Left != nil {
		Walk(t.Left, ch)
	}
	ch <- t.Value
	if t.Right != nil {
		Walk(t.Right, ch)
	}
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	ch1 := make(chan int)
	ch2 := make(chan int)
	go func() {
		Walk(t1, ch1)
		close(ch1)
	}()
	go func() {
		Walk(t2, ch2)
		close(ch2)
	}()
	for {
		v1, ok1 := <-ch1
		if !ok1 {
			_, ok2 := <-ch2
			if ok2 {
				return false
			}
			break
		}
		v2, ok2 := <-ch2
		if !ok2 {
			return false
		}
		fmt.Printf("- %v %v\n", v1, v2)
		if v1 != v2 {
			return false
		}
	}
	return true
}

func main() {
	sa := Same(tree.New(1), tree.New(1))
	fmt.Printf("%v\n", sa)
}
