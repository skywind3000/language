package main

import (
	"fmt"
)

func main() {
	colors := map[string]string{
		"AliceBlue":   "#f0f8ff",
		"Coral":       "#ff7f50",
		"DarkGray":    "#a9a9a9",
		"ForestGreen": "#228b22",
	}

	delete(colors, "Coral")

	for key, value := range colors {
		fmt.Printf("Key: %s Value: %s\n", key, value)
	}
}
