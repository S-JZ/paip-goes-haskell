package main

import (
	"fmt"
	"strings"
)

const space = " "
const star = "*"
const plus = "+"
const dash = "-"
const clrf = "\n"

func spaces(row int) string {
	return strings.Repeat(space, row)
}

func triangle(row int, syml string) string {
	return strings.Repeat(syml, row)
}

func pyramidLine(row int, syml string) string {
	return strings.Repeat(syml, 2*row+1)
}

func pyramid(sz int, syml string) string {
	p := ""
	for i := 0; i < sz; i++ {
		p += spaces(sz - i)
		p += pyramidLine(i, syml)
		p += clrf
	}
	return p
}

func main() {
	fmt.Println(pyramid(5, star))
	fmt.Println(pyramid(5, plus))
	fmt.Println(pyramid(5, dash))
}
