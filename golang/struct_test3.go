package main

import (
	"fmt"
)

type user struct {
	name       string
	email      string
	ext        int
	privileged bool
}

type admin struct {
	person user
	level  string
}

func main() {
	fred := admin{
		person: user{
			name:       "Fred",
			email:      "fred@email.com",
			ext:        123,
			privileged: true,
		},
		level: "super",
	}
	fmt.Println(fred)
}
