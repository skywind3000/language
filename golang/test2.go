package main

import (
	"fmt"
	"os"
)

func main() {
	dir, _ := os.UserConfigDir();
	fmt.Println("hello, world", dir);
}


