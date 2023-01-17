// Count Atoms in an expression
//
// Write a function that counts the number of atoms in an expression.
// For example: (count-atoms ' ( a (b) c ) ) = 3. Notice that there is something of an
// ambiguity in this: should (a nil c) count as three atoms, or as two, because it is
// equivalent to (a () c) ?
//
// LISP:
// 		(defun count-atoms (exp)
// 		  "Return the total number of non-nil atoms in the expression."
// 		  (cond ((null exp) 0)
// 			((atom exp) 1)
// 				(t (+ (count-atoms (first exp))
// 				(count-atoms (rest exp))))))
//
// 		(defun count-all-atoms (exp &optional (if-null 1))
// 		  "Return the total number of atoms in the expression,
// 		   counting nil as an atom only in non-tail position."
// 		   (cond ((null exp) if-null)
// 				((atom exp) 1)
// 					(t (+ (count-all-atoms (first exp) 1)
// 						  (count-all-atoms (rest exp) 0))))
//

package main

import (
	"fmt"
	"strings"
)

func first(s string) string {
	return s[0:1]
}

func last(s string) string {
	return s[len(s)-1:]
}

func rest(s string) string {
	return s[1:]
}

func isEmpty(exp string) bool {
	return exp == ""
}

func countAtoms(exp string) int {
	if isEmpty(exp) {
		return 0
	}

	if isAtom(exp) {
		return 1
	}

	return countAtoms(first(exp)) + countAtoms(rest(exp))
}

func isAtom(exp string) bool {
	return first(exp) != "(" && first(exp) != ")"
}

func stripParentheses(exp string) string {
	if first(exp) == "(" && last(exp) == ")" {
		return exp[1 : len(exp)-1]
	}
	return exp
}

func getAtoms(exp string) []string {
	if isEmpty(exp) {
		return []string{}
	}

	atoms := strings.Split(exp, " ")

}

func countAllAtoms(exp string, ifNull int) int {
	if isEmpty(exp) {
		return ifNull
	}

	if isAtom(exp) {
		return 1
	}

	return countAllAtoms(first(exp), 1) + countAllAtoms(rest(exp), 0)
}

func main() {
	fmt.Println(getAtoms("a (b) c"))
}
