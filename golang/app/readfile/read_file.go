package main

import (
	"fmt"
	"io/ioutil"
	"github.com/goinaction/code/chapter3/words"
)

func main() {
	file, err := ioutil.ReadFile("read_file.go")
	if err != nil {
		fmt.Println("Error reading file")
		return
	}
	fmt.Println(string(file))
	text := string(file)
	count := words.CountWords(text)
	fmt.Printf("There are %d words in the text.\n", count)
}


