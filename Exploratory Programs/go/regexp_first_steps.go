package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Sample code from gobyexample.com/regular-expressions
	match1, _ := regexp.MatchString("p([a-z]+)ch", "peach")
	fmt.Println(match1)
	match2, _ := regexp.MatchString("([a-z]+)", "24525")
	fmt.Println(match2)
	// So the value returned is a boolean

	// Trying to match the regex string with an actual string
	// to get the values we need from the text
	// FindAllStringSubmatch will help us get the
	// successive matches of the expression in a slice
	pattern, err := regexp.Compile(`^If (.*), then (.*)`)

	if err != nil {
		fmt.Println(err)
	}

	text := "If Tom has 4 bananas, then he bought them"
	match := pattern.FindAllStringSubmatch(text, -1)
	matchedVariables := match[0][1:]
	fmt.Printf("%q\n", matchedVariables)

	// Take a regex, take a string, and convert it to an expression
	
}
