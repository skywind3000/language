package main

import (
	"os"
	"encoding/gob"
)


func main() {
	info := map[string]string {
		"name" : "lilei",
		"age" : "24",
	}
	fp, _ := os.OpenFile("test.gob", os.O_RDWR | os.O_CREATE, 644)
	enc := gob.NewEncoder(fp)
	if err := enc.Encode(info); err != nil {
		println(err)
	}
	pos, _ := fp.Seek(0, 1)
	println("current", pos)
	if err := enc.Encode(info); err != nil {
		println(err)
	}
	pos, _ = fp.Seek(0, 1)
	println("current", pos)

	fp.Close()

	var m map[string]string

	fp, _ = os.Open("test.gob")
	dec := gob.NewDecoder(fp)
	if err := dec.Decode(&m); err != nil {
		println(err)
	}
	
	pos, _ = fp.Seek(0, 1)
	println(pos)
}


