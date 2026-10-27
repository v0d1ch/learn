// range1
// Make me compile!
//
// I AM NOT DONE
package main

import "fmt"

func main() {
	evenNumbers := []int{2, 4, 6, 8, 9, 10}

	for _, v := range evenNumbers {
    	if v % 2 == 0 {
		   fmt.Printf("%d is even\n", v)
	    } else {
		   fmt.Printf("%d is NOT even\n", v)

        }
	}
}
