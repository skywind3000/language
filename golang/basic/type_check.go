package main

import "fmt"

func check(i interface{}) {
	if i == nil {
		fmt.Printf("check: obj is nil\n")
	} else {
		switch v := i.(type) {
		case int:
			fmt.Printf("Twice %v is %v\n", v, v*2)
		case string:
			fmt.Printf("%q is %v bytes long\n", v, len(v))
		case map[string]interface{}:
			fmt.Printf("map[string]interface{}: %v\n", v)
		default:
			fmt.Printf("I don't know about type %T!\n", v)
		}
	}
}

func main() {
	check(21)
	check("hello")
	check(true)
	check(nil)
	var i1 interface{} = nil
	var i2 interface{} = 32
	var i3 map[string]interface{} = make(map[string]interface{})
	var i4 map[string]interface{} = nil
	check(i1)
	check(i2)
	check(i3)
	check(i4)
}

/* OUTPUT:
Twice 21 is 42
"hello" is 5 bytes long
I don't know about type bool!
check: obj is nil
check: obj is nil
Twice 32 is 64
*/
