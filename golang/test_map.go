package main


func main() {
	m := map[string]string {}
	t := m["test"]
	println("size", len(t))
	if t == "" {
		println("empty")
	}	else {
		println("not empty")
	}
}
