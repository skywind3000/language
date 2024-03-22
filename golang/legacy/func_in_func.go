package main

func main() {
	var x = 1
	foo := func () {
		println("foo is", x)
		x++
	}
	bar := func () {
		println("bar is", x)
		x++
	}
	foo()
	bar()
	foo()
	bar()
}

