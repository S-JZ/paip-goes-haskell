// Count Anywhere
//
// Write a function that counts the number of times an expression
// occurs anywhere within another expression. Example: (count-anywhere 'a *(a ((a) b) a)) = 3.
//
// 	(defun count-anywhere (item tree)
// 		"Count the times item appears anywhere within tree."
// 		(cond ((eql item tree) 1)
// 			((atom tree) 0)
// 			(t (+ (count-anywhere item (first tree))
// 				  (count-anywhere item (rest tree))))))
//

package main

import "fmt"

func first(s string) string {
	return s[0:1]
}

func rest(s string) string {
	return s[1:]
}

func countAnywhere(item string, tree string) int {
	if tree == "" {
		return 0
	}

	if item == first(tree) {
		return 1 + countAnywhere(item, rest(tree))
	}

	return countAnywhere(item, rest(tree))
}

func main() {
	fmt.Println(countAnywhere("a", "*(a ((a) b) a)"))
	fmt.Println(countAnywhere("a", "*(a ((a) b) a)"))
	fmt.Println(countAnywhere("b", "*(a ((a) b)"))
	fmt.Println(countAnywhere("c", "*(a ((a) b)"))
}
