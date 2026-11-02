// generics1
// Make me compile!

// I AM NOT DONE
package main

import "fmt"

func main() {
	print("Hello, World!")
	print(42)
}

func print[T string | int](value T) {
	fmt.Println(value)
}
