---
description: Little Excerpts from PAIP
---

# Objectives

> _Eliza is best thought of not as a program that understands natural language, but as a program that tricks people into thinking that it understands natural language._                                                            &#x20;

By implementing Eliza in Haskell, the idea was to explore the power and elegance of functional programming while also diving into the history of natural language processing.



## Describing and Specifying Eliza

The Eliza algorithm can be described simply as:

1. Read an input
2. Find a pattern that matches the input
3. Transform the input into a response
4. Print the response.&#x20;

These four steps are repeated for each input.

## Pattern Matching

Pattern matching was the core of the Eliza algorithm, as it allowed the program to analyze the user's input and generate an appropriate response. The program used a set of pattern-matching rules to identify certain keywords or phrases in the user's input and transform them into appropriate responses.

For example,

Pattern: (i need a X)&#x20;

Response: (what would it mean to you if you got a X ? )

The original Eliza program was written in LISP, a functional programming language known for its powerful list-processing capabilities and support for symbolic expressions. LISP's support for pattern matching and symbolic manipulation made it an ideal choice for developing the Eliza program.

In order to implement it in Haskell, the syntax used to define patterns needed to be thoroughly studied. In LISP, pattern matching is often accomplished through the use of "destructuring" macros that allow the programmer to extract specific elements from lists or other data structures. In Haskell, pattern matching is integrated directly into the language syntax, with patterns specified using a set of specialized syntax rules.

## Eliza Rules

> Now that we know what an individual rule will do, we need to decide how to handle a set of rules. If ELIZA is to be of any interest, it will have to have a variety of responses. So several rules may all be applicable to the same input. One possibility would be to choose a rule at random from among the rules having patterns that match the input. Another possibility is just to accept the first rule that matches.

In the Haskell implementation of Eliza, the rules are an ordered list in such a way that it takes advantage of this ordering and arranges for the most specific rules to come first, while more vague rules are near the end of the list.

{% hint style="info" %}
The original ELIZA had a system where each rule had a priority number associated with it. The matching rule with the highest priority was chosen
{% endhint %}
