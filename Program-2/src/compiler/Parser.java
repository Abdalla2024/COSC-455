//  ************** REQUIRES JAVA 21 or later! (https://adoptium.net/) ************** //
/*
COURSE: COSC455-002
Assignment: Program 1
Name: Abdelmagid, Abdalla
*/

package compiler;

import java.util.logging.Logger;
import java.util.HashSet;
import java.util.Set;

/*
 Important: Understanding the relationship between the comments below (the grammar) and the code
 below is critical to understanding how to implement a recursive descent parser!

 GRAMMAR FOR PROCESSING SIMPLE SENTENCES:

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


    Note: This example violates the Java "coding conventions" that method names should be all lower case.
          but this is done to make the code more readable by exactly matching the names of grammar rules.
==================================================================================================================

NOTE: The ***PARSER_README.MD*** file has complete information, but here are some highlights:

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

    // Symbol table to track declared variables and functions
    private final Set<String> declaredVariables = new HashSet<>();
    private final Set<String> declaredFunctions = new HashSet<>();

    // ///////////////////////////////////////////////////////////////////////////////////////////////////
    // !!!!!!!!!!!!!!!!!!! CODE MODIFICATIONS SHOULD BE MADE BELOW THIS LINE
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // ///////////////////////////////////////////////////////////////////////////////////////////////////

    // <START> ::= <PROGRAM> | <SENTENCE> $$
    private void START(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<START>");

        // Check if this is a program or a sentence
        if (isStmtStart(lexer.getCurrentToken())) {
            // This is a program
            PROGRAM(thisNode);
        } else {
            // This is a sentence
            SENTENCE(thisNode);
            MATCH(thisNode, TokenSet.$$);
        }
    }

    // Boolean function to check if the current token is a statement start
    private boolean isStmtStart(TokenSet token) {
        return token == TokenSet.READ ||
                token == TokenSet.WRITE ||
                token == TokenSet.VAR ||
                token == TokenSet.LET ||
                token == TokenSet.DEF ||
                token == TokenSet.IF ||
                token == TokenSet.FOR ||
                token == TokenSet.ID; // For function calls
    }

    // <PROGRAM> ::= <STMT_LIST> $$
    private void PROGRAM(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<PROGRAM>");

        STMT_LIST(thisNode);
        MATCH(thisNode, TokenSet.$$);
    }

    // <STMT_LIST> ::= <STMT> <STMT_LIST> | ε
    private void STMT_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<STMT_LIST>");

        // If the current token is a statement start, parse a statement and then parse
        // the rest of the statement list
        if (isStmtStart(lexer.getCurrentToken())) {
            STMT(thisNode);
            STMT_LIST(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <STMT> ::= <READ_STMT> | <WRITE_STMT> | <VAR_DECL> | <LET_STMT> | <FUNC_DEF>
    // | <IF_STMT> | <FOR_LOOP>
    private void STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<STMT>");

        if (lexer.getCurrentToken() == TokenSet.READ) {
            READ_STMT(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.WRITE) {
            WRITE_STMT(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.VAR) {
            VAR_DECL(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.LET) {
            LET_STMT(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.DEF) {
            FUNC_DEF(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.IF) {
            IF_STMT(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.FOR) {
            FOR_LOOP(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.ID) {
            // Save the current lexeme and token
            String currentLexeme = lexer.getCurrentLexeme();
            TokenSet currentToken = lexer.getCurrentToken();

            // Match the ID token
            MATCH(thisNode, TokenSet.ID);

            // Check the next token to determine if it's a function call or assignment
            if (lexer.getCurrentToken() == TokenSet.OPEN_PAREN) {
                // It's a function call
                FUN_CALL(thisNode);
            } else if (lexer.getCurrentToken() == TokenSet.EQUAL) {
                // It's a variable assignment - create a new ASSIGNMENT node
                final TreeNode assignNode = codeGenerator.addNonTerminal(thisNode, "<ASSIGNMENT>");
                // Add the ID to the assignment node
                codeGenerator.addTerminal(assignNode, TokenSet.ID, currentLexeme);
                // Match the equal sign
                MATCH(assignNode, TokenSet.EQUAL);
                // Parse the expression
                EXPRESSION(assignNode);
            } else {
                // Invalid statement
                codeGenerator.syntaxError(thisNode, "Expected '(' for function call or '=' for assignment");
            }
        }
    }

    // <READ_STMT> ::= <READ> <ID>
    private void READ_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<READ_STMT>");

        MATCH(thisNode, TokenSet.READ);
        MATCH(thisNode, TokenSet.ID);
    }

    // <WRITE_STMT> ::= <WRITE> <EXPRESSION>
    private void WRITE_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<WRITE_STMT>");

        MATCH(thisNode, TokenSet.WRITE);
        EXPRESSION(thisNode);
    }

    // <LET_STMT> ::= <LET> <ID> <LET_TAIL>
    private void LET_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<LET_STMT>");

        MATCH(thisNode, TokenSet.LET);
        String varName = lexer.getCurrentLexeme();
        MATCH(thisNode, TokenSet.ID);
        checkVariableDeclared(varName, thisNode);
        LET_TAIL(thisNode);
    }

    // <VAR_DECL> ::= <VAR> <ID> "=" <EXPRESSION>
    private void VAR_DECL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<VAR_DECL>");

        MATCH(thisNode, TokenSet.VAR);
        String varName = lexer.getCurrentLexeme();
        MATCH(thisNode, TokenSet.ID);
        addVariableDeclaration(varName, thisNode);
        MATCH(thisNode, TokenSet.EQUAL);
        EXPRESSION(thisNode);
    }

    // <FUN_CALL> ::= <ID> '(' <ARG_LIST> ')'
    private void FUN_CALL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<FUN_CALL>");

        String funcName = lexer.getCurrentLexeme();
        MATCH(thisNode, TokenSet.ID);
        checkFunctionDeclared(funcName, thisNode);
        MATCH(thisNode, TokenSet.OPEN_PAREN);
        ARG_LIST(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_PAREN);
    }

    // <LET_TAIL> ::= "=" <EXPRESSION> | "<-" <FUN_CALL>
    private void LET_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<LET_TAIL>");

        if (lexer.getCurrentToken() == TokenSet.EQUAL) {
            MATCH(thisNode, TokenSet.EQUAL);
            EXPRESSION(thisNode);
        } else {
            MATCH(thisNode, TokenSet.ARROW);
            FUN_CALL(thisNode);
        }
    }

    // <ARG_LIST> ::= <EXPRESSION> <ARGS_TAIL> | ε
    private void ARG_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ARG_LIST>");

        if (isExprStart(lexer.getCurrentToken())) {
            EXPRESSION(thisNode);
            ARGS_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // Boolean function to check if the current token is an expression start
    private boolean isExprStart(TokenSet token) {
        return token == TokenSet.ADD_OP || token == TokenSet.ID || token == TokenSet.NUMBER
                || token == TokenSet.OPEN_PAREN;
    }

    // <ARGS_TAIL> ::= <COMMA> <ARG_LIST> | ε
    private void ARGS_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ARGS_TAIL>");

        if (lexer.getCurrentToken() == TokenSet.COMMA) {
            MATCH(thisNode, TokenSet.COMMA);
            ARG_LIST(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <EXPRESSION> ::= <TERM> <TERM_TAIL>
    private void EXPRESSION(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<EXPRESSION>");

        TERM(thisNode);
        TERM_TAIL(thisNode);
    }

    // <TERM> ::= <FATOR> <FACTOR_TAIL>
    private void TERM(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<TERM>");

        FACTOR(thisNode);
        FACTOR_TAIL(thisNode);
    }

    // <TERM_TAIL> ::= <ADD_OP> <TERM> <TERM_TAIL> | ε
    private void TERM_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<TERM_TAIL>");

        if (lexer.getCurrentToken() == TokenSet.ADD_OP) {
            MATCH(thisNode, TokenSet.ADD_OP);
            TERM(thisNode);
            TERM_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <FACTOR> ::= "-" <FACTOR> | "(" <EXPRESSION> ")" | <ID> | <NUM>
    private void FACTOR(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<FACTOR>");

        if (lexer.getCurrentToken() == TokenSet.ADD_OP) {
            MATCH(thisNode, TokenSet.ADD_OP);
            FACTOR(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.OPEN_PAREN) {
            MATCH(thisNode, TokenSet.OPEN_PAREN);
            EXPRESSION(thisNode);
            MATCH(thisNode, TokenSet.CLOSE_PAREN);
        } else if (lexer.getCurrentToken() == TokenSet.ID) {
            MATCH(thisNode, TokenSet.ID);
        } else if (lexer.getCurrentToken() == TokenSet.NUMBER) {
            MATCH(thisNode, TokenSet.NUMBER);
        }
    }

    // <FACTOR_TAIL> ::= <MULT_OP> <FACTOR> <FACTOR_TAIL> | ε
    private void FACTOR_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<FACTOR_TAIL>");

        if (lexer.getCurrentToken() == TokenSet.MULT_OP) {
            MATCH(thisNode, TokenSet.MULT_OP);
            FACTOR(thisNode);
            FACTOR_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <IF_STMT> ::= <IF> "(" <CONDITION> ")" "{" <STMT_LIST> "}" <ELSE_STMT>
    private void IF_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<IF_STMT>");

        MATCH(thisNode, TokenSet.IF);
        MATCH(thisNode, TokenSet.OPEN_PAREN);
        CONDITION(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_PAREN);
        MATCH(thisNode, TokenSet.OPEN_BRACE);
        STMT_LIST(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_BRACE);
        ELSE_STMT(thisNode);
    }

    // <CONDITION> ::= <EXPRESSION> <REL_OP> <EXPRESSION>
    private void CONDITION(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<CONDITION>");

        EXPRESSION(thisNode);
        MATCH(thisNode, TokenSet.REL_OP);
        EXPRESSION(thisNode);
    }

    // <ELSE_STMT> ::= <ELSE> "{" <STMT_LIST> "}" | ε
    private void ELSE_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ELSE_STMT>");

        if (lexer.getCurrentToken() == TokenSet.ELSE) {
            MATCH(thisNode, TokenSet.ELSE);
            MATCH(thisNode, TokenSet.OPEN_BRACE);
            STMT_LIST(thisNode);
            MATCH(thisNode, TokenSet.CLOSE_BRACE);
        } else {
            EPSILON(thisNode);
        }
    }

    // <FUNC_DEF> ::= "DEF" <ID> "(" <PARAM_LIST> ")" "{" <STMT_LIST> "RETURN"
    // <EXPRESSION> "}"
    private void FUNC_DEF(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<FUNC_DEF>");

        MATCH(thisNode, TokenSet.DEF);
        String funcName = lexer.getCurrentLexeme();
        MATCH(thisNode, TokenSet.ID);
        addFunctionDeclaration(funcName, thisNode);
        MATCH(thisNode, TokenSet.OPEN_PAREN);
        PARAM_LIST(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_PAREN);
        MATCH(thisNode, TokenSet.OPEN_BRACE);
        STMT_LIST(thisNode);
        MATCH(thisNode, TokenSet.RETURN);
        EXPRESSION(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_BRACE);
    }

    // <PARAM_LIST> ::= <ID> <PARAM_TAIL> | ε
    private void PARAM_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<PARAM_LIST>");

        if (lexer.getCurrentToken() == TokenSet.ID) {
            MATCH(thisNode, TokenSet.ID);
            PARAM_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <PARAM_TAIL> ::= <COMMA> <ID> <PARAM_TAIL> | ε
    private void PARAM_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<PARAM_TAIL>");

        if (lexer.getCurrentToken() == TokenSet.COMMA) {
            MATCH(thisNode, TokenSet.COMMA);
            MATCH(thisNode, TokenSet.ID);
            PARAM_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <FOR_LOOP> ::= "FOR" "(" <VAR_DECL> ";" <CONDITION> ";" <ASSIGNMENT> ")" "{"
    // <STMT_LIST> "}"
    private void FOR_LOOP(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<FOR_LOOP>");

        MATCH(thisNode, TokenSet.FOR);
        MATCH(thisNode, TokenSet.OPEN_PAREN);
        VAR_DECL(thisNode);
        MATCH(thisNode, TokenSet.SEMICOLON);
        CONDITION(thisNode);
        MATCH(thisNode, TokenSet.SEMICOLON);
        ASSIGNMENT(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_PAREN);
        MATCH(thisNode, TokenSet.OPEN_BRACE);
        STMT_LIST(thisNode);
        MATCH(thisNode, TokenSet.CLOSE_BRACE);
    }

    // <ASSIGNMENT> ::= <ID> "=" <EXPRESSION>
    private void ASSIGNMENT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<ASSIGNMENT>");

        // Get the variable name before matching the ID token
        String varName = lexer.getCurrentLexeme();
        MATCH(thisNode, TokenSet.ID);

        // Check if the variable is declared
        if (!declaredVariables.contains(varName)) {
            codeGenerator.syntaxError(thisNode, "Variable '" + varName + "' used before declaration");
        }

        MATCH(thisNode, TokenSet.EQUAL);
        EXPRESSION(thisNode);
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

    // <SENTENCE_TAIL> ::= <CONJ> <SENTENCE> | <EOS>
    private void SENTENCE_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<SENTENCE_TAIL>");

        // Check for a conjunction.
        if (lexer.getCurrentToken() == TokenSet.CONJUNCTION) {
            MATCH(thisNode, TokenSet.CONJUNCTION);
            SENTENCE(thisNode);
        } else {
            MATCH(thisNode, TokenSet.PERIOD);
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
        if (lexer.getCurrentToken() == TokenSet.LIST_SEPARATOR) {
            MATCH(thisNode, TokenSet.LIST_SEPARATOR);
            MATCH(thisNode, TokenSet.ADJECTIVE);
            ADJ_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <VERB_PHRASE> ::= <ADVERB> <VERB> <OPT_PREPOSITION> | <VERB>
    // <OPT_PREPOSITION>
    private void VERB_PHRASE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<VERB_PHRASE>");

        // Check for an adverb.
        if (lexer.getCurrentToken() == TokenSet.ADVERB) {
            MATCH(thisNode, TokenSet.ADVERB);
        }

        // Match the verb.
        MATCH(thisNode, TokenSet.VERB);

        // Check for an optional preposition.
        if (lexer.getCurrentToken() == TokenSet.PREPOSITION) {
            MATCH(thisNode, TokenSet.PREPOSITION);
        }
    }

    // <PREP_PHRASE> ::= <PREPOSITION> <NOUN_PHRASE> | ε
    private void PREP_PHRASE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminal(parentNode, "<PREP_PHRASE>");

        // Check for a preposition.
        if (lexer.getCurrentToken() == TokenSet.PREPOSITION) {
            MATCH(thisNode, TokenSet.PREPOSITION);
            NOUN_PHRASE(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    /// //////////////////////////////////////////////////////////////////////////////////

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
            var errorMessage = "SYNTAX ERROR: Expected '%s'\nbut found '%s' (%s)."
                    .formatted(expectedToken, currentLexeme, currentToken);
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

    // Helper method to check if a variable is declared
    private void checkVariableDeclared(String varName, TreeNode parentNode) throws ParseException {
        if (!declaredVariables.contains(varName)) {
            codeGenerator.syntaxError(parentNode,
                    String.format("Variable '%s' used before declaration", varName));
        }
    }

    // Helper method to check if a function is declared
    private void checkFunctionDeclared(String funcName, TreeNode parentNode) throws ParseException {
        if (!declaredFunctions.contains(funcName)) {
            codeGenerator.syntaxError(parentNode,
                    String.format("Function '%s' used before declaration", funcName));
        }
    }

    // Helper method to add a variable declaration
    private void addVariableDeclaration(String varName, TreeNode parentNode) throws ParseException {
        if (declaredVariables.contains(varName)) {
            codeGenerator.syntaxError(parentNode,
                    String.format("Variable '%s' already declared", varName));
        }
        declaredVariables.add(varName);
    }

    // Helper method to add a function declaration
    private void addFunctionDeclaration(String funcName, TreeNode parentNode) throws ParseException {
        if (declaredFunctions.contains(funcName)) {
            codeGenerator.syntaxError(parentNode,
                    String.format("Function '%s' already declared", funcName));
        }
        declaredFunctions.add(funcName);
    }

    // The lexer, which will provide the tokens
    private final LexicalAnalyzer lexer;

    // The "code generator"
    private final CodeGenerator codeGenerator;
}
