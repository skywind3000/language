package main

import (
	"encoding/json"
	"log"
)

var JSON = `{
	"name": "Gopher",
	"title": "programmer",
	"contact": {
		"home": "415.333.3333",
		"cell": "415.555.5555"
	}
}
`

func main() {
	var obj interface{}
	err := json.Unmarshal([]byte(JSON), &obj)
	if err != nil {
		log.Printf("ERROR: %s\n", err)
		return
	}
	if obj == nil {
		log.Println("decode: obj is nil")
		return
	}
	if m, ok := obj.(map[string]interface{}); ok {
		log.Printf("name: %s\n", m["name"])
		log.Printf("title: %s\n", m["title"])
		if contact, ok := m["contact"].(map[string]interface{}); ok {
			log.Printf("home: %s\n", contact["home"])
			log.Printf("cell: %s\n", contact["cell"])
		}
	}
}
