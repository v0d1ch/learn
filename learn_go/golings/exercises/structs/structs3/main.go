// structs3
// Make me compile!
//
// I AM NOT DONE
package main

import "fmt"

type Person struct {
	firstName  string
	lastName   string
	FullName   func () string
}

func main() {
	person := Person{FullName: func() string {return "Maurício Antunes"}}
	fmt.Printf("Person full name is: %s\n", person.FullName()) // here it must output Person full name is: Maurício Antunes
}
