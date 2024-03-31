package main

import (
	"image"
	"image/color"

	"golang.org/x/tour/pic"
)

type Image struct {
	w int
	h int
}

func (self *Image) ColorModel() color.Model {
	return color.RGBAModel
}

func (self *Image) Bounds() image.Rectangle {
	return image.Rect(0, 0, self.w, self.h)
}

func (self *Image) At(x int, y int) color.Color {
	c := x ^ y
	v := byte(c)
	return color.RGBA{v, v, 255, 255}
}

func main() {
	m := Image{256, 256}
	pic.ShowImage(&m)
}
