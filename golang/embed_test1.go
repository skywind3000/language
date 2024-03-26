package main

import (
	"fmt"
)

type user struct {
	name  string
	email string
}

func (u *user) notify() {
	fmt.Printf("Sending user email to %s<%s>\n", u.name, u.email)
}

type admin struct {
	user
	level string
}

func main() {
	ad := admin{
		user: user{
			name:  "John Smith",
			email: "john@email.com",
		},
		level: "super",
	}
	ad.user.notify()
	ad.notify()
}
