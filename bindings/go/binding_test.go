package tree_sitter_buzz_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-buzz"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_buzz.Language())
	if language == nil {
		t.Errorf("Error loading Buzz grammar")
	}
}
