//  ************** REQUIRES Java 21 or later! (https://adoptium.net/) ************** //
/*
COURSE: COSC455-002
Assignment: Program 0
Name: Abdelmagid, Abdalla
 
(Pairs are peremitted, but no more than two in a "group".  Each student should submit individually.)
*/

package compiler;

import java.util.logging.Logger;

/*
 Important: Understanding the relationship between the comments below (the grammar) and the code
 below is critical to understanding how to implement a recursive descent parser!

 GRAMMAR FOR PROCESSING SIMPLE SENTENCES:

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


NOTES: The ***PARSER_README.MD*** file has complete information, but here are some highlights:

 1. Each method implements a production rule.

 2. Each production-implementing method starts by adding itself to the tree via the codeGenerator.
    See the code below for examples of usage.

 3. The Code Generator implements:

       addNonTerminal(parentNode, displayName)
            — Adds a non-terminal node to the tree and returns the node that was added.

       addTerminal(parentNode, token, displayName)
            — Adds a terminal node to the tree and returns the node that was added.

       addEpsilon(parentNode, displayName)
            — Adds an empty node to the tree and returns the node that was added.

       syntaxError(parentNode, errorMessage)
            — Throws a ParseException with the given message and adds exception to the tree.

  4. The Lexical Analyzer Class implements:

       getCurrentToken()
            — Returns the current token.

       getCurrentLexeme()
            — Returns the current string that maps to the token.

       advanceToken()
            — Advances to the next token by setting the "current token" to the "next" token.
              If the end of the token list is reached, the current token is "$$" (end of file).

  5. The sample already includes the following methods which can be used WITHOUT modification:

       EPSILON(TreeNode parentNode)
            — Implements the epsilon production rule.
              (See Textbook Section 2.3.1)

       MATCH(Token token, TreeNode parentNode)
            — Matches the current token with the expected token.
              (See Textbook Section 2.3.1)
              
 */

class Parser {

    // ///////////////////////////////////////////////////////////////////////////////////////////////////
    // !!!!!!!!!!!!!!!!!!! CODE MODIFICATIONS SHOULD BE MADE BELOW THIS LINE
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // ///////////////////////////////////////////////////////////////////////////////////////////////////

    // <START> :== <SENTENCE> $$
    // NOTE: The name of this method should not change unless you also change the
    // invocation from "analyze" below.
    private void START(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<START>");

        // Invoke the first rule.
        SENTENCE(thisNode);

        // Test for the end of input ($$ meta token).
        MATCH(thisNode, TokenSet.$$);
    }

    // <SENTENCE> ::= <NOUN_PHRASE> <VERB_PHRASE> <NOUN_PHRASE> <PREP_PHRASE>
    // <SENTENCE_TAIL>
    private void SENTENCE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<SENTENCE>");

        // Invoke the rules for the sentence.
        NOUN_PHRASE(thisNode);
        VERB_PHRASE(thisNode);
        NOUN_PHRASE(thisNode);
        PREP_PHRASE(thisNode);
        SENTENCE_TAIL(thisNode);
    }

    // <SENTENCE_TAIL> ::= 'and' <SENTENCE> | 'or' <SENTENCE> | <PUNCTUATION>
    private void SENTENCE_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<SENTENCE_TAIL");

        if (lexer.getCurrentToken() == TokenSet.CONJUNCTION) {
            MATCH(thisNode, TokenSet.CONJUNCTION);
            SENTENCE(thisNode);
        } else {
            MATCH(thisNode, TokenSet.PUNCTUATION);
        }

    }

    // <NOUN_PHRASE> ::= <ART> <ADJ_LIST> <NOUN>
    private void NOUN_PHRASE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<NOUN_PHRASE>");

        // Invoke the rules for the noun phrase.
        MATCH(thisNode, TokenSet.ARTICLE);
        ADJ_LIST(thisNode);
        MATCH(thisNode, TokenSet.NOUN);
    }

    // <ADJ_LIST> ::= <ADJECTIVE> <ADJ_TAIL> | ε
    private void ADJ_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ADJ_LIST>");

        // Check for an adjective.
        if (lexer.getCurrentToken() == TokenSet.ADJECTIVE) {
            MATCH(thisNode, TokenSet.ADJECTIVE);
            ADJ_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <ADJ_TAIL> ::= <COMMA> <ADJECTIVE> <ADJ_TAIL> | ε
    private void ADJ_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ADJ_TAIL>");

        // Check for a list separator.
        if (lexer.getCurrentToken() == TokenSet.COMMA) {
            MATCH(thisNode, TokenSet.COMMA);
            MATCH(thisNode, TokenSet.ADJECTIVE);
            ADJ_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <VERB_PHRASE> ::= <ADVERB> <VERB> | <VERB>
    private void VERB_PHRASE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<VERB_PHRASE>");

        // Check for an adverb.
        if (lexer.getCurrentToken() == TokenSet.ADVERB) {
            MATCH(thisNode, TokenSet.ADVERB);
        }

        // Match the verb.
        MATCH(thisNode, TokenSet.VERB);
    }

    // <PREP_PHRASE> ::= <PREPOSITION> <NOUN_PHRASE> | ε
    private void PREP_PHRASE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "PREP_PHRASE");

        if (lexer.getCurrentToken() == TokenSet.PREPOSITION) {
            MATCH(thisNode, TokenSet.PREPOSITION);
            NOUN_PHRASE(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // ///////////////////////////////////////////////////////////////////////////////////

    /*
     * THIS CAN BE USED AS IS! Though you may modify it to suit your needs.
     *
     * Add an EPSILON terminal node (result of an Epsilon Production) to the parse
     * tree.
     * Mainly, this is just done for better visualizing the complete parse tree.
     *
     * @param parentNode The parent of the terminal node.
     */
    private void EPSILON(final TreeNode parentNode) {
        codeGenerator.addEpsilon(parentNode);
    }

    /*
     * THIS CAN BE USED AS IS! Though you may modify it to suit your needs.
     *
     * Match the current token with the expected token.
     * If they match, add the token to the parse tree, otherwise throw an exception.
     *
     * @param currentNode The current terminal node.
     * 
     * @param expectedToken The token to be matched.
     * 
     * @throws ParseException Thrown if the token does not match the expected token.
     */
    private void MATCH(final TreeNode currentNode, final TokenSet expectedToken) throws ParseException {
        // Get the current token and lexeme.
        var currentToken = lexer.getCurrentToken();
        var currentLexeme = lexer.getCurrentLexeme();

        // Check if the current token matches the expected token.
        if (currentToken == expectedToken) {
            codeGenerator.addTerminal(currentNode, currentToken, currentLexeme);
            lexer.advanceToken();
        } else {
            // If the current token does not match the expected token, throw an exception.
            var errorMessage = "SYNTAX ERROR: Expected '%s' but found '%s' (%s).".formatted(expectedToken,
                    currentLexeme, currentToken);
            codeGenerator.syntaxError(currentNode, errorMessage);
        }
    }

    // ////////////////////////////////////////////////////////////////////////////////////////////////
    // !!!!!!!!!!!!!!!!!!! CODE MODIFICATIONS SHOULD BE MADE ABOVE THIS LINE
    // !!!!!!!!!!!!!!!!!!!!!!!!!
    // ////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * THIS IS INVOKED DIRECTLY FROM MAIN AND SHOULD NOT BE MODIFIED.
     *
     * @param lexer         The TokenSet Object
     * @param codeGenerator The CodeGenerator Object
     */
    Parser(LexicalAnalyzer lexer, CodeGenerator codeGenerator) {
        this.lexer = lexer;
        this.codeGenerator = codeGenerator;

        // !!!! Change this to automatically prompt to see the Open WebGraphViz dialog
        // or not. !!!!
        MAIN.PROMPT_FOR_GRAPHVIZ = true;
    }

    /**
     * THIS IS INVOKED DIRECTLY FROM MAIN AND SHOULD NOT BE MODIFIED.
     * <p>
     * Since the "Compiler" portion of the code knows nothing about the start rule,
     * the "analyze" method must invoke the start rule.
     *
     * @param treeRoot The tree root.
     */
    public void analyze(TreeNode treeRoot) {
        try {
            // THIS IS OUR START RULE
            START(treeRoot);
        } catch (ParseException ex) {
            final String msg = String.format("%s\n", ex.getMessage());
            Logger.getAnonymousLogger().severe(msg);
        }
    }

    // The lexer, which will provide the tokens
    private final LexicalAnalyzer lexer;

    // The "code generator"
    private final CodeGenerator codeGenerator;
}
