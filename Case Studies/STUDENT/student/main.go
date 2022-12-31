// STUDENT - An Early AI Program that solves simple arithmetic problems
// The following is the implementation of the STUDENT program in Go
package main

import (
	"fmt"
	"regexp"
	"strings"
	"github.com/marcak/calc/calc"
)

// Rule represents a pattern and an expression to be applied to the pattern
type Rule struct {
	pattern    string
	expression string
}

// mapStrStr short alias for map[string]string
type mapStrStr map[string]string

// matchPatterns recursively matches patterns to the input, starting
// with the topmost pattern, and then works its way down nested patterns,
// if found.
//
// It takes two parameters:
//
//	rules   - a list of rules to match against
//	toMatch - a list of strings to match against the rules
//
// toMatch is a list of strings because the input may contain nested
// patterns, and we need to match them from the outermost pattern to the
// innermost pattern.
//
// For example, if the input is "what is the sum of 3 and 4", then the
// parsing will be done as follows, adding each match to toMatch:
//
//	what is 3 plus 4
//		├── what
//		└── 3 plus 4
//				├── 3
//				└── 4
//
// While matching, it also builds a list of equations in prefix notation
// and a map of variables to their full expanded form.  For example, if
// the input is "what is 3 plus 4", then the prefixEqns list
// will contain the following:
//
// 		[(= what plus) (+ 3 4)]
//
// Thus, it returns two values:
// 		prefixEqns - a list of parsed equations in prefix notation
// 		expanded   - a map of variables to their full expanded form
// 
// expanded is a map of variables mapped to their full expanded form.
// At the moment, variables are simply named by choosing the first 
// non-numerical word in the matched string.  For example, if the input
// is "what is the sum of 3 and 4", then the expanded map will contain
// the following:
//
// 		{
//			"what": "(= what plus)", 
//			"plus": "(+ 3 4)"
//		}
//
// The use of the expanded map is to relate a nested expression to its
// parent equation. In the above prefixEqns list, we see that
// (+ 3 4) is just an expression, and it is not an equation with an
// assignment.  However, we can use the expanded map to find the
// assignment for the expression, which is (= what plus).  This is
// important because we need to know the assignment in order to
// substitute the expression into the equation, and consequently
// evaluate the equation.
func matchPatterns(rules []Rule, toMatch []string) ([]string, mapStrStr) {
	prefixEqns := make([]string, 0)
	expanded := make(mapStrStr)
	haveMatched := make(map[string]bool)

	for len(toMatch) > 0 {
		input := toMatch[0]
		matchFound := false
		for _, rule := range rules {
			matches, ok := matchRule(input, rule)
			if ok {
				applyRule(rule, input, matches, &prefixEqns, &expanded)
				addIfNotPresent(matches, &toMatch, &haveMatched)
				toMatch = toMatch[1:]
				matchFound = true
				break
			}
		}

		if !matchFound {
			toMatch = toMatch[1:]
		}
	}

	return prefixEqns, expanded
}

// matchRule matches a string against one rule.  
// 
// If the string matches the pattern specified in the rule, 
// then it returns the matched parts of the string, and a 
// boolean to denote that a match was found.  Otherwise, 
// it returns an empty list and false.
//
// Parameters:
// 		input - the string to match against the rule
// 		rule  - the rule to match against the string
//
// Returns:
// 		matches - a list of matched parts of the string
// 		ok      - boolean denoting whether a match was found
func matchRule(input string, rule Rule) ([]string, bool) {
	pattern, err := regexp.Compile(rule.pattern)
	if err != nil {
		panic(err)
	}

	matches := pattern.FindStringSubmatch(input)
	if len(matches) >= 2 {
		return matches[1:], true
	}

	return []string{}, false
}

// applyRule applies one rule to the input string.
//
// It takes four parameters:
// 		rule     - the rule to apply
// 		matched  - the string that was matched by the rule
// 		matches  - the matched parts of the string
// 		eq       - a list of equations in prefix notation
// 		expanded - a map of variables to their full expanded form
//
// The function substitutes the matched parts of the string into the
// expression specified by the rule, and then adds the expression to
// the list of equations given by `eq`.
//
// The string `match` is used to create a variable name for the 
// expression, which is as the shorthand for the expression in the
// expanded map. For example, if the input is "what is 3 plus 4", then:
// 		`matched` will be "3 plus 4", and 
// 		`matches` will be ["3", "4"].
// `matches` will be substituted into the expression, which in this
// case is "(+ ?x ?y)".  The resulting expression will be "(+ 3 4)",
// which will be added to the list of equations given by `eq`.
//
// `expanded` will use `matched` to determine the variable name (or)
// the key for the expression. Thus, expanded[variable-name] = expression.
func applyRule(rule Rule, matched string, matches []string, eq *[]string, expanded *mapStrStr) {
	// some rules only have one variable to match,
	// so we use a conditional to check for that
	expression := strings.Replace(rule.expression, "?x", makeVariable(matches[0]), -1)
	if len(matches) > 1 {
		expression = strings.Replace(expression, "?y", makeVariable(matches[1]), -1)
	}

	if !strings.HasPrefix(rule.expression, "?") {
		*eq = append(*eq, expression)
		(*expanded)[makeVariable(matched)] = expression
	}
}

// bindVariables takes a list of equations in infix notation and
// returns a map of variables to their values.
//
// Parameters:
//
//	eqns - a list of equations in infix notation
//
// Returns:
//
//	variables - a map of variables to their values
//
// The purpose of this function is to extract the variables from
// the infix notation and in a way, "assign" them to their values.
// For example, if the input is:
//
//	{
//		"what = plus",
//		"plus = (+ 3 4)"
//	}
//
// Then the output will be:
//
//	{
//		"what": "plus",
//	    "plus": "(+ 3 4)"
//	}
func bindVariables(eqns []string) map[string]string {
	variables := make(map[string]string)
	for _, eq := range eqns {
		eq = trimParantheses(eq)
		if strings.Contains(eq, "=") {
			lhs, rhs, found := strings.Cut(eq, " = ")
			if !found {
				panic(eq)
			}
			variables[lhs] = rhs
		}
	}

	return variables
}

// substituteVariables takes a list of equations in infix notation
// represented as a map, and substitutes the variables in the equations
// with their values.
//
// Parameters:
//
//	eqns - a map of equations in infix notation
//
// Returns:
//
//	eqns - same map, but with variables substituted
//
// It substitutes the values of the variables in the equations until
// no more substitutions are possible.  For example, if the input is:
//
//	{
//		"what": "plus",
//		"plus": "(+ 3 4)"
//	}
//
// Then the output will be:
//
//	{
//		"what": "(+ 3 4)"
//	    "plus": "(+ 3 4)"
//	}
func substituteVariables(eqns map[string]string) map[string]string {
	// Keep track of whether any substitutions were made
	madeSubstitution := true

	// Repeat the substitution process until no more substitutions are possible
	for madeSubstitution {
		madeSubstitution = false

		// Iterate over the eqns
		for lhs, rhs := range eqns {
			// Iterate over the variables in the equation
			for variable, value := range eqns {
				// If the variable is in the equation, substitute it with its value
				if strings.Contains(rhs, variable) {
					eqns[lhs] = strings.Replace(rhs, variable, value, -1)
					madeSubstitution = true
				}
			}
		}
	}

	return eqns
}

// prefixToInfix takes a prefix notation expression and converts
// it to infix notation. It uses a stack to perform the conversion.
//
// Parameters:
//
//	exp - a prefix notation expression
//
// Returns:
//
//	infixExp - the same expression in infix notation
func prefixToInfix(exp string) string {
	var stack []string

	elements := strings.Fields(removeAllParantheses(exp))
	for i := len(elements) - 1; i >= 0; i-- {
		element := elements[i]
		if isOperator(element) || element == "=" {
			lhs := stackTop(&stack)
			stackPop(&stack)
			rhs := stackTop(&stack)
			stackPop(&stack)
			infixExp := addParantheses(lhs + " " + element + " " + rhs)
			stackPush(&stack, infixExp)
		} else {
			stackPush(&stack, element)
		}
	}

	return stackTop(&stack)
}

// Auxiliary functions to support the operations of the STUDENT program

// completeEquations takes an expression and a map of expansions and
// completes the expression if it is incomplete.
//
// Incomplete expressions are simply expressions that are not part of
// an equation.
//
// For example, for the problem: "what is 3 plus 4", the parsed
// expression of "3 plus 4" is (+ 3 4). This expression is incomplete
// because it is not part of an equation.

// Thus, completeEquations will complete the expression by assigning
// it to an appropriate variable and adding it to the map of
// expansions for future use.
func completeEquations(exp *string, expanded mapStrStr) {
	elements := strings.Fields(trimParantheses(*exp))
	if elements[0] != "=" {
		expansion, ok := getExpansion(*exp, expanded)
		if ok {
			*exp = "(" + "= " + expansion + " " + *exp + ")"
		}
	}
}

// addIfNotPresent adds the matched parts to the toMatch slice if they
// have not been parsed before. This is to prevent the same part from
// being parsed multiple times, as well as to add any new parts that
// were not parsed before, if it happens to be a pattern itself.
//
// Parameters:
//
//	matches     - list of strings, each of which is a part of the
//			      input that matched a pattern
//	toMatch     - pointer to the slice of strings to be parsed
//	haveMatched - pointer to a map of strings to booleans, which
//				  keeps track of which strings have been parsed
//
// Returns:
//
//	none, but toMatch and haveMatched are modified in place
func addIfNotPresent(matches []string, toMatch *[]string, haveMatched *map[string]bool) {
	for _, match := range matches {
		if !(*haveMatched)[match] {
			(*haveMatched)[match] = true
			*toMatch = append(*toMatch, match)
		}
	}
}

// removeNoiseWords removes words that are not significant
// to the problem. It iterates over the words in the input
// and removes the words that are not significant.
//
// Parameters:
//
//	input - a string of words
//
// Returns:
//
//	noiseFree - the same string, but free of noise words
func removeNoiseWords(input string) string {
	words := strings.Split(input, " ")
	var noiseFree []string
	for _, w := range words {
		if !isNoiseWord(strings.ToLower(w)) {
			noiseFree = append(noiseFree, w)
		}
	}
	return strings.Join(noiseFree, " ")
}

// makeVariable takes a string and returns the most
// significant word in the string. This is used to
// create a variable name for the expression.
//
// Currently, we assume an arbitrary variable name
// and thus we simply return the first word in the
// string that is not a number or special character(s).
func makeVariable(match string) string {
	words := strings.Split(match, " ")
	if len(words) > 1 {
		if !isNumber(words[0]) {
			return words[0]
		} else {
			return words[1]
		}
	}

	return match
}

// getExpansion takes an expression and a map of expansions and
// returns the key in the map that corresponds to the expression.
//
// This is used to get the variable name for the expression.
//
// Parameters:
//
//	exp      - the expression to find the expansion for
//	expanded - the map of expansions
//
// Returns:
//
//	key - the key in the map that corresponds to the expression
//	ok  - true if the key was found, false otherwise
func getExpansion(exp string, expanded mapStrStr) (string, bool) {
	for k, v := range expanded {
		if exp == v {
			return k, true
		}
	}
	return "", false
}

// Miscellaneous helper functions

// isNoiseWord checks if a word is a noise word.
func isNoiseWord(w string) bool {
	noiseWords := []string{"a", "an", "the", "this", "number", "of", "to", "\\'s"}
	for _, nw := range noiseWords {
		if w == nw {
			return true
		}
	}
	return false
}

// isNumber checks if a string is a number.
func isNumber(s string) bool {
	reg := regexp.MustCompile(`^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$`)
	return reg.MatchString(s)
}

// isOperator checks if a string is an operator.
func isOperator(op string) bool {
	operators := []string{"+", "-", "*", "/", "^", "%"}
	for _, o := range operators {
		if op == o {
			return true
		}
	}
	return false
}

// trimParantheses removes the outermost parantheses
// from an expression.
func trimParantheses(exp string) string {
	exp = strings.TrimPrefix(exp, "(")
	exp = strings.TrimSuffix(exp, ")")
	return exp
}

// addParantheses adds parantheses to the ends of an expression.
func addParantheses(exp string) string {
	return "(" + exp + ")"
}

// removeAllParantheses removes all occurences of
// parantheses from an expression.
func removeAllParantheses(exp string) string {
	exp = strings.Replace(exp, "(", "", -1)
	exp = strings.Replace(exp, ")", "", -1)
	return exp
}

// Helper functions for stack operations -----------------------------

// stackPush pushes an element onto the stack.
func stackPush(stk *[]string, s string) {
	*stk = append(*stk, s)
}

// stackPop pops the top element off the stack.
func stackPop(stk *[]string) {
	*stk = (*stk)[:len(*stk)-1]
}

// stackTop returns the top element of the stack.
func stackTop(stk *[]string) string {
	return (*stk)[len(*stk)-1]
}

func main() {
	/*
		OTHER TEST CASES:
		The price of a radio is 69.70 dollars ,., If this price is 15 %% less than the marked price , find the marked price ,.,
		If the number of customers Tom gets is twice the square of 20 %% of the number of advertisements he runs, and the number of advertisements is 45 , and the profit Tom receives is 10 times the number of customers he gets , then what is the profit
		The average score is 73 , The maximum score is 97 ,., What is the square of the difference between the average and the maximum
		Tom is twice Mary's age , and Jane's age is half the difference between Mary and Tom ,., If Mary is 18 years old , how old is Jane
		What is 4 + 5 * 1 4 / 7 ?
		x * b = c + d ,., b * c = x ,., x = b + b ,., b = 5
	*/

	problem := "If the number of customers Tom gets is twice the square of 20 %% of the number of advertisements he runs and the number of advertisements is 45 then what is the number of customers Tom gets"
	// problem = `The price of a radio is 69.70 dollars . If this price is 15 %% less than the marked price , find the marked price`
	// problem = "If the number of customers Tom gets is twice the square of 20 %% of the number of advertisements he runs, and the number of advertisements is 45 , and the profit Tom receives is 10 times the number of customers he gets , then what is the profit"
	// problem = "Tom is twice Mary \\'s age , and Jane \\'s age is half the difference between Mary and Tom \\. If Mary is 18 years old , how old is Jane"
	// problem = "What is 4 + 5 * 14 / 7 ?"
	// problem = "x * b = c + d . b * c = x . x = b + b . b = 5"
	problem = removeNoiseWords(problem)

	rules := []Rule{
		{pattern: "(.*) \\. (.*)", expression: "?x ?y"},
		{pattern: "(.*) \\.", expression: "?x"},
		{pattern: "(.*) , (.*)", expression: "?x ?y"},
		{pattern: "If (.*) , then (.*)", expression: "?x ?y"},
		{pattern: "If (.*) then (.*)", expression: "?x ?y"},
		{pattern: "If (.*) , (.*)", expression: "?x ?y"},
		{pattern: "(.*) and (.*)", expression: "?x ?y"},
		{pattern: "find (.*) and (.*)", expression: "(= toFind ?x) (= toFind ?y))"},
		{pattern: "find (.*)", expression: "(= toFind ?x)"},
		{pattern: "(.*) equals (.*)", expression: "(= ?x ?y)"},
		{pattern: "(.*) same as (.*)", expression: "(= ?x ?y)"},
		{pattern: "(.*) = (.*)", expression: "(= ?x ?y)"},
		{pattern: "(.*) is equal to (.*)", expression: "(= ?x ?y)"},
		{pattern: "(.*) is (.*)", expression: "(= ?x ?y)"},
		{pattern: "(.*) - (.*)", expression: "(- ?x ?y)"},
		{pattern: "(.*) minus (.*)", expression: "(- ?x ?y)"},
		{pattern: "difference between (.*) and (.*)", expression: "(- ?x ?y)"},
		{pattern: "difference (.*) and (.*)", expression: "(- ?x ?y)"},
		{pattern: "(.*) + (.*)", expression: "(+ ?x ?y)"},
		{pattern: "(.*)* plus (.*)*", expression: "(+ ?x ?y)"},
		{pattern: "sum (.*) and (.*)", expression: "(+ ?x ?y)"},
		{pattern: "product (.*) and (.*)", expression: "(* ?x ?y)"},
		{pattern: "(.*) \\* (.*)", expression: "(* ?x ?y)"},
		{pattern: "(.*) times (.*)", expression: "(* ?x ?y)"},
		{pattern: "(.*) / (.*)", expression: "(/ ?x ?y)"},
		{pattern: "(.*) per (.*)", expression: "(/ ?x ?y)"},
		{pattern: "(.*) divided by (.*)", expression: "(/ ?x ?y)"},
		{pattern: "half (.*)", expression: "(/ ?x 2)"},
		{pattern: "one half (.*)", expression: "(/ ?x 2)"},
		{pattern: "twice (.*)", expression: "(* ?x 2)"},
		{pattern: "square (.*)", expression: "(* ?x ?x)"},
		{pattern: "(.*) %% less than (.*)", expression: "(* ?y (/ (- 100 ?x) 100))"},
		{pattern: "(.*) %% more than (.*)", expression: "(* ?y (/ (+ 100 ?x) 100))"},
		{pattern: "(.*) %% (.*)", expression: "(* (/ ?x 100) ?y)"},
	}

	toMatch := []string{problem}
	prefixEqns, expanded := matchPatterns(rules, toMatch)

	fmt.Println("\nEquations in Prefix Notation:\n-------")
	for _, eq := range prefixEqns {
		fmt.Println(eq)
	}

	infixEqns := make([]string, 0)
	for _, eq := range prefixEqns {
		completeEquations(&eq, expanded)
		infixEqns = append(infixEqns, prefixToInfix(eq))
	}

	fmt.Println("\nEquations to Solve:\n-------")
	for _, eq := range infixEqns {
		fmt.Println(eq)
	}

	vars := substituteVariables(bindVariables(infixEqns))
	fmt.Printf("\nAfter substitution:\n-------")
	for k, v := range vars {
		fmt.Printf("\n%s = %s", k, v)
	}

	fmt.Println("\n\nSolutions:\n-------")
	for v, eq := range vars {
		fmt.Println(v, "=", calc.Solve(eq))
	}
}
