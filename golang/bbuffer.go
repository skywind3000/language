package main

import (
	"bytes"
	"encoding/gob"
)

func main() {
	m := map[string]string {
		"name": "lilei",
		"age": "24",
	}
	var b bytes.Buffer
	e := gob.NewEncoder(&b)
	e.Encode(m)
	println(len(b.Bytes()))

	nb := bytes.NewBuffer(b.Bytes())
	d := gob.NewDecoder(nb)
	var x map[string]string 
	d.Decode(&x)
	println(x["name"], x["age"])
}


