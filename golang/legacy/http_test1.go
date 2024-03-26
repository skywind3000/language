package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	var url string = "https://www.baidu.com"
	r, err := http.Get(url)
	if err != nil {
		panic(err)
	}
	io.Copy(os.Stdout, r.Body)
	if err := r.Body.Close(); err != nil {
		panic(err)
	}
}
