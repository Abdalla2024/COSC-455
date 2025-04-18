//  ************** REQUIRES Java 21 OR ABOVE! (https://adoptium.net/) ************** //
/*
COURSE: COSC455-002
Assignment: Program 1
Name: Abdelmagid, Abdalla
*/

package compiler;

import java.util.Arrays;
import java.util.List;

/**
 * Remember this is part of a "fake" tokenizer, that when handed a string, it
 * simply resolves to a
 * TOKEN object matching that string. All the Tokens/Terminals Used by the
 * parser. The purpose of
 * the enum type here is to eliminate the need for direct character comparisons.
 * <p>
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!<br>
 * -----------------------------------------------------------------------------<br>
 * IN *MOST* REAL CASES, THERE WILL BE ONLY ONE LEXEME PER compiler TokenSet!
 * <p>
 * The fact that several lexemes exist per token in this example is because this
 * is to parse simple
 * In English sentences, most of the token types have many words (lexemes) that
 * could fit.
 * *** This is generally NOT the case in most programming languages!!! ***
 */

public enum TokenSet {
    // Sentence-related tokens
    ARTICLE("a", "the"),
    NOUN("dog", "cat", "rat", "tree", "fox"),
    VERB("jumps", "chases"),
    ADJECTIVE("quick", "brown", "slow", "lazy", "tall"),
    LIST_SEPARATOR(","),
    ADVERB("quickly", "slowly"),
    PREPOSITION("around", "up", "over"),
    CONJUNCTION("and", "or"),
    PERIOD("."),
    EXCLAMATION("!"),

    // Programming language tokens
    READ("read"),
    WRITE("write"),
    VAR("var"),
    LET("let"),
    DEF("def"),
    RETURN("return"),
    IF("if"),
    ELSE("else"),
    FOR("for"),
    ID("id"),

    // Operators
    ADD_OP("+", "-"),
    MULT_OP("*", "/"),
    REL_OP("<", ">", "<=", ">=", "=="),
    EQUAL("="),
    ARROW("<-"),

    // Punctuation
    OPEN_PAREN("("),
    CLOSE_PAREN(")"),
    OPEN_BRACE("{"),
    CLOSE_BRACE("}"),
    COMMA(","),
    SEMICOLON(";"),

    // Special tokens
    NUMBER,
    $$, // End of file
    UNIDENTIFIED_TOKEN; // For unknown identifiers

    /**
     * A list of all lexemes for each token.
     */
    private final List<String> lexemeList;

    /**
     * Constructor for the TokenSet enum.
     * Accepts a variable number of lexemes for each token. However,
     * in MOST programming languages, there is only one lexeme per token.
     *
     * @param tokenStrings The lexemes for the token.
     */
    TokenSet(final String... tokenStrings) {
        // lowercase all lexemes and collect them into an Arraylist.
        this.lexemeList = Arrays
                .stream(tokenStrings)
                .map(String::toLowerCase)
                .toList();
    }

    /*
     * Get a TokenSet object from the Lexeme string.
     *
     * @param string The String (lexeme) to convert to a compiler.TokenSet
     * 
     * @return A compiler.TokenSet object based on the input String (lexeme)
     */
    static TokenSet getTokenFromLexeme(final String string) {
        // Just to be safe…
        final var lexeme = string.trim().toLowerCase();

        // An empty string/lexeme should mean no more tokens to process.
        // Return the "end of input maker" if the string is empty.
        if (lexeme.isEmpty()) {
            return $$;
        }

        // First check for specific tokens (keywords, operators, etc.)
        for (var token : TokenSet.values()) {
            if (token.lexemeList.contains(lexeme)) {
                // early bailout from the method within the loop.
                return token;
            }
        }

        // Then check for numbers
        if (lexeme.matches(LexicalAnalyzer.NUMBER_REGEX)) {
            return NUMBER;
        }

        // Finally check for generic identifiers
        if (lexeme.matches("[a-zA-Z][a-zA-Z0-9_]*")) {
            return ID;
        }

        // Return "UNIDENTIFIED_TOKEN" if no match was found.
        return UNIDENTIFIED_TOKEN;
    }
}
