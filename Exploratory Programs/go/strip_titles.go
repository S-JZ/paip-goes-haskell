package main

import (
	"errors"
	"fmt"
	"os"
	"strings"
)

func makeHashSet[T comparable](list []T) map[T]bool {
	hashSet := make(map[T]bool)
	for _, element := range list {
		hashSet[element] = true
	}
	return hashSet
}

func stripTitles(nameAsList []string, titleSet map[string]bool) string {
	if len(nameAsList) == 0 {
		return ""
	}

	if titleSet[nameAsList[0]] {
		return stripTitles(nameAsList[1:], titleSet)
	}

	return strings.Join(nameAsList, " ")
}

func main() {
	titles := []string{"Ms", "Miss", "Mr", "Master", "Dr", "Mrs", "Col", "Colonel", "Gen", "General", "Capt", "Captain", "Major", "Admiral", "Sir", "Madam"}
	titleSet := makeHashSet(titles)

	args := os.Args[1:]
	if len(args) < 1 {
		fmt.Println(errors.New("No input provided"))
		os.Exit(1)
	} else {
		var name []string
		if len(args) == 1 {
			name = strings.Split(args[0], " ")
		} else {
			name = args
		}

		fmt.Println(stripTitles(name, titleSet))
	}
}
