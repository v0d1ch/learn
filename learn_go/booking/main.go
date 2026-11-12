package main

import (
    "fmt"
    "strings"
)

func main(){
    const conferenceName = "Go conference"
    const conferenceTickets = 50
	var remainingTickets uint = 50
    fmt.Printf("Welcome to %v booking app!\n", conferenceName)
    fmt.Println("We have total of", conferenceTickets, "remaining", remainingTickets)
    fmt.Println("You can get your tickets here")

    var firstName string
    var lastName string
    var email string
	var userTickets uint 
	bookings := []string{}
    for {
        fmt.Println("Enter your first name")
        fmt.Scan(&firstName)
        fmt.Println("Enter your last name")
        fmt.Scan(&lastName)
        fmt.Println("Enter your email")
        fmt.Scan(&email)
        fmt.Println("Enter number of tickets")
        fmt.Scan(&userTickets)

		if userTickets <= remainingTickets {
	        bookings = append(bookings,firstName + " " + lastName)

	        fmt.Printf("Thank you %v %v! You purchased %v tickets.\n", firstName, lastName, userTickets)
            remainingTickets = remainingTickets - userTickets
	        fmt.Printf("%v tickets remaining for %v\n", remainingTickets, conferenceName)

		    var firstNames []string

		    for _, name := range bookings {
		    	var fst = strings.Fields(name)
		    	firstNames = append(firstNames, fst[0])
		    }

	        fmt.Printf("Guests %v\n", firstNames)

		    if remainingTickets == 0 {
		    	fmt.Println("Conference fully booked")
		    	break
		    }
		} else {
			fmt.Printf("You can't but %v tickets - there is only %v remainingTickets\n", userTickets, remainingTickets)
			continue
		}

	}


}
