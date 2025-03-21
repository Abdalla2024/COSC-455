<!-- This is a "Markdown" File (https://www.markdownguide.org).  While "Markdown" is a
     format designed to be both human and machine-readable, having a Markdown displayer
     plugin/extension will provide a nicer reading experience. 

     IMPORTANT: YOU CAN IGNORE THE `MAIN.java` FILE!
     The only relevant code is in `Parser.java` and `TokenSet.java` files.
-->

# _This is a sample recursive descent parser!_

The language is defined by the following grammar:

``` BNF
   <START> ::= <SENTENCE> $$
   <SENTENCE> ::= <NOUN_PHRASE> <VERB_PHRASE> <NOUN_PHRASE> <PERIOD>

   <NOUN_PHRASE> ::= <ARTICLE> <ADJ_LIST> <NOUN>
   <VERB_PHRASE> ::= <ADVERB> <VERB> | <VERB>
   <ADJ_LIST> ::= <ADJECTIVE> <ADJ_TAIL> | ε
   <ADJ_TAIL> ::= <COMMA> <ADJECTIVE> <ADJ_TAIL> | ε

   <ARTICLE> ::= 'a' | 'the'
   <NOUN> ::= 'dog' | 'cat' | 'rat' | 'fox' |  'tree' | 'house'
   <VERB> ::= 'jumps' |  'chases' | 'climbs'
   <ADJECTIVE> ::= 'fast' | 'slow' | 'lazy' | 'tall'
   <ADVERB> ::= 'quickly' | 'quietly'
   <COMMA> ::= ','
   <PERIOD> ::= '.'
```

---

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
