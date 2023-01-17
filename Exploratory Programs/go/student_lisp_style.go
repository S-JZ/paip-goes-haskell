// First Attempt at STUDENT - an AI Algebraic Problem Solver
//
// Tried to implement the STUDENT algorithm in the Lisp-style
// of pattern matching here. Got rid of this approach because it was
// too complicated and hard to extend.

package main

import (
	"fmt"
	"reflect"
	"strings"
)

type exp struct {
	op  rune
	lhs interface{}
	rhs interface{}
}

type rule []string

var toMatch [][]string

func isNoiseWord(w string) bool {
	noiseWords := []string{"a", "an", "the", "is", "number", "of"}
	for _, nw := range noiseWords {
		if w == nw {
			return true
		}
	}
	return false
}

func removeNoiseWords(problem []string) []string {
	var clean []string
	for _, w := range problem {
		if !isNoiseWord(w) {
			clean = append(clean, w)
		}
	}
	return clean
}

// isVariable returns true if the given token is a variable (starts with "?").
func isVariable(token string) bool {
	return strings.HasPrefix(token, "?")
}

// isSegment returns true if the given token is a segment variable
// (starts with "?*").
func isSegment(token string) bool {
	return strings.HasPrefix(token, "?*")
}

// containsTokens returns true if the given list of tokens contains at least
// one variable or segment variable.
func containsTokens(tokens []string) bool {
	for _, token := range tokens {
		if isVariable(token) || isSegment(token) {
			return true
		}
	}
	return false
}

// matchSegment matches the given segment variable against the input
func matchSegment(variable string, pattern []string, input []string, bindings map[string][]string, start int) map[string][]string {
	// If there are no words in pattern following var, we can just match var
	// to the remainder of the input.
	if len(pattern) == 0 {
		return matchVariable(variable, input, bindings)
	}

	// Get the segment boundary word and look for the first occurrence in
	// the input starting from index start.
	word := pattern[0]
	pos := -1
	for i := start; i < len(input); i++ {
		if input[i] == word {
			pos = i
			break
		}
	}

	// When the boundary word doesn't appear in the input, no match.
	if pos == -1 {
		return nil
	}

	// Match the located substring to the segment variable and
	// recursively pattern match using the resulting bindings.
	varMatch := matchVariable(variable, input[:pos], map[string][]string{})
	match := matchPattern(pattern, input[pos:], varMatch)

	// If pattern matching fails with this substring, try a longer one.
	if match == nil {
		return matchSegment(variable, pattern, input, bindings, start+1)
	}

	return match
}

func makeVariable(variable string) string {
	words := strings.Split(variable, " ")
	return words[0]
}

// matchVariable binds the input to the variable and updates the bindings.
// Returns the updated bindings or false if the variable is already bound
// to a different input.
func matchVariable(variable string, replacement []string, bindings map[string][]string) map[string][]string {
	binding, ok := bindings[variable]

	// The variable isn't bound yet
	if !ok {
		bindings[makeVariable(variable)] = replacement
		return bindings
	}

	// The variable is already bound to that input
	if reflect.DeepEqual(replacement, binding) {
		return bindings
	}

	// The variable is already bound, but not to that input -- which is a fail
	return nil
}

func matchPattern(pattern []string, input []string, bindings map[string][]string) map[string][]string {
	// Check if matching has failed before reaching this point
	// Initialize bindings if not provided
	if bindings == nil {
		bindings = make(map[string][]string)
	}

	// If pattern and input are identical, we have a match and no more
	// bindings need to be found
	if reflect.DeepEqual(pattern, input) {
		return bindings
	}

	// Match input to pattern
	if isSegment(pattern[0]) {
		// Segment variable is of the form ?*x
		variable := pattern[0][2:]
		return matchSegment(variable, pattern[1:], input, bindings, 0)
	}

	if isVariable(pattern[0]) {
		// Variable is of the form ?x
		variable := pattern[0][1:]
		return matchVariable(variable, input, bindings)
	}

	// Recurse:
	// try to match the first tokens of both pattern and input.
	// The bindings that result are used to match the remainder
	// of both lists.
	if pattern[0] == input[0] {
		return matchPattern(pattern[1:], input[1:], bindings)
	}
	return nil
}

func main() {
	problem := "If Tom runs 45 advertisements, and the number of customers he gets is twice the square of 20%% of the number of advertisements he runs, then what is the number of customers Tom gets?"
	problemList := strings.Split(problem, " ")
	problemList = removeNoiseWords(problemList)

	rules := [][]string{
		{"?*x", "."},
		{"?*x", ".", "?*y"},
		{"If", "?*x", ",", "then", "?*y"},
		{"If", "?*x", "then", "?*y"},
		{"If", "?*x", ",", "?*y"},
		{"?*x", ",", "and", "?*y"},
		{"find", "?*x", "and", "?*y"},
		{"find", "?*x"},
		{"?*x", "equals", "?*y"},
		{"?*x", "same as", "?*y"},
		{"?*x", "=", "?*y"},
		{"?*x", "is equal to", "?*y"},
		{"?*x", "is", "?*y"},
		{"?*x", "-", "?*y"},
		{"?*x", "minus", "?*y"},
		{"difference", "between", "?*x", "and", "?*y"},
		{"difference", "?*x", "and", "?*y"},
		{"?x*", "+", "?y*"},
		{"?x*", "plus", "?y*"},
		{"sum", "?*x", "and", "?*y"},
		{"product", "?*x", "and", "?*y"},
		{"?*x", "*", "?*y"},
		{"?*x", "times", "?*y"},
		{"?*x", "/", "?*y"},
		{"?*x", "per", "?*y"},
		{"?*x", "divided", "by", "?*y"},
		{"half", "?*x"},
		{"one", "half", "?*x"},
		{"twice", "?*x"},
		{"double", "?*x"},
		{"?*x", "%%", "less", "than", "?*y"},
		{"?*x", "%%", "more", "than", "?*y"},
		{"?*x", "%%", "?*y"},
	}

	pattern := []string{"?*x", "likes", "?*y"}
	input := []string{"Jon", "Bellion", "likes", "apples", "and", "bananas"}
	fmt.Printf("%q\n", problemList)
	bindings := matchPattern(pattern, input, nil)
	fmt.Println(bindings)
	fmt.Println(len(rules))
}
