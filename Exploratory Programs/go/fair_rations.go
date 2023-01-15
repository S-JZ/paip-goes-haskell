package main

import (
	"fmt"
	"strconv"
)

func isEven(n int) bool {
	return n%2 == 0
}

func transform(loaves []int) string {
	var s string
	for _, l := range loaves {
		if isEven(l) {
			s += "e"
		} else {
			s += "o"
		}
	}
	return s
}

func oddOdds(B []int32) int32 {
	var count int32
	for _, b := range B {
		if b%2 == 1 {
			count++
		}
	}
	return count
}

func fairRations(loaves []int32) string {
	if oddOdds(loaves)%2 == 1 {
		return "NO"
	}

	var count int32
	for i := 0; i < len(loaves)-1; i++ {
		if loaves[i]%2 == 1 {
			loaves[i]++
			loaves[i+1]++
			count += 2
		}
	}

	return strconv.Itoa(int(count))
}

func main() {
	fmt.Println(fairRations([]int32{2, 3, 4, 5, 6}))
	fmt.Println(fairRations([]int32{1, 2}))
}
