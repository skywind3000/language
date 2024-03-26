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

func main() {
	var bill user
	bill.ext = 10
	bill.name = "bill"
	bill.email = "bill@163.com"
	bill.notify()
	var b2 *user = new(user)
	b2.name = "zhj"
	b2.email = "zhj@163.com"
	b2.notify()
}
