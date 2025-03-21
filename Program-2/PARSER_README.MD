# This is a sample recursive descent parser example

It is a simple parser that can parse a simple language.
The language is defined by the following grammar:

```
    BNF For the Language:
    
    <START> ::= <SENTENCE> $$
    <SENTENCE> ::= <NOUN_PHRASE> <VERB_PHRASE> <NOUN_PHRASE> <PREP_PHRASE> <SENTENCE_TAIL>
    <SENTENCE_TAIL> ::= 'and' <SENTENCE> | '.' | '!'
    
    <NOUN_PHRASE> ::= <ARTICLE> <ADJ_LIST> <NOUN>
    <ADJ_LIST> ::= <ADJECTIVE> <ADJ_TAIL> | ε
    <ADJ_TAIL> ::= <LIST_SEPARATOR> <ADJECTIVE> <ADJ_TAIL> | ε
    
    <VERB_PHRASE> ::= <ADVERB> <VERB> <OPT_PREPOSITION> | <VERB> <OPT_PREPOSITION>
    <OPT_PREPOSITION> ::= <PREPOSITION> | ε
    <PREP_PHRASE> ::= <PREPOSITION> <NOUN_PHRASE> | ε
    
    <ARTICLE> ::= 'a' | 'the'
    <NOUN> ::= 'dog' | 'cat' | 'rat' | 'tree' | 'fox'
    <VERB> ::= 'jumps' |  'chases'
    <ADJECTIVE> ::= 'quick' | 'brown' | 'slow' | 'lazy' | 'tall'
    <LIST_SEPARATOR> ::= ','
    <ADVERB> ::= 'quickly' | 'slowly'
    <PREPOSITION> ::= 'around' | 'up' | 'over'
```


## In this Sample:

1. Each method implements a production rule.
2. Each production-implementing method starts by adding itself to the tree via the `codeGenerator`.

---

## Available Methods (the API):

The Code Generator implements:

       addNonTerminal(parentNode, displayName)
            — Adds a non-terminal node to the tree and returns the node that was added.

       addTerminal(parentNode, token, displayName)
            — Adds a terminal node to the tree and returns the node that was added.

       addEpsilon(parentNode, displayName)
            — Adds an empty node to the tree and returns the node that was added.

       syntaxError(parentNode, errorMessage)
            — Throws a ParseException with the given message and adds exception to the tree.

The Lexical Analyzer Class implements:

       getCurrentToken()
            — Returns the current token.

       getCurrentLexeme()
            — Returns the current string that maps to the token.

       advanceToken()
            — Advances to the next token by setting the "current token" to the "next" token.
              If the end of the token list is reached, the current token is "$$" (end of file).

---

### Notes on `Parser.java`

       The sample already includes the following methods which can be used WITHOUT modification:

       EPSILON(TreeNode parentNode)
            — Implements the epsilon production rule.
              (See Textbook Section 2.3.1)


       MATCH(Token token, TreeNode parentNode)
            — Matches the current token with the expected token.
              (See Textbook Section 2.3.1)
