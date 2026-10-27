// structs1
// Make me compile!
//
// I AM NOT DONE
package main

import "fmt"

type Sport struct {
  name string
}
type Person struct {
	name string
	age int
	sport []Sport

}

func main() {
	person := Person {name: "Sasha", age: 49, sport: []Sport{{"snowboard"}}}
	fmt.Printf("Person %s and age %d and sport %s", person.name, person.age, person.sport)
}
