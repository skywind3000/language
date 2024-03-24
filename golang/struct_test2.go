package main

type user struct {
	name       string
	email      string
	ext        int
	privileged bool
}

func (self *user) notify() {
	println("Sending user email to", self.name, self.email)
}

func newUser(name string) *user {
	var nu user
	nu.name = name
	return &nu
}

func main() {
	var n1 *user = newUser("user1")
	var n2 *user = newUser("user2")
	n1.notify()
	n2.notify()
}
