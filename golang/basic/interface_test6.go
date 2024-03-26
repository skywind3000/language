package main

import (
	"fmt"
	"reflect"
)

func getInterface(what string) interface{} {
	switch what {
	case "int":
		return (interface{})(1)
	case "string":
		return (interface{})("hello")
	case "bool":
		return (interface{})(true)
	}
	return nil
}

func main() {
	var obj interface{}

	obj = getInterface("int")
	fmt.Printf("obj: %v, type: %T\n", obj, reflect.TypeOf(obj))

	x, ok := obj.(int)
	if ok {
		fmt.Printf("value: %v\n", x)
	}

	obj = getInterface("string")
	fmt.Printf("obj: %v, type: %T\n", obj, reflect.TypeOf(obj))

	y, ok := obj.(string)
	if ok {
		fmt.Printf("value: %v\n", y)
	}

	obj = getInterface("other")
	if obj == nil {
		fmt.Println("obj is nil")
	}
}
