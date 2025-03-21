//  ************** REQUIRES Java 21 OR ABOVE! (https://adoptium.net/) ************** //
/*
COURSE: COSC455-002
Assignment: Program 0
Name: Abdelmagid, Abdalla
 
(Pairs are peremitted, but no more than two in a "group".  Each student should submit individually.)
*/

package compiler;

import java.util.Arrays;
import java.util.List;

/*
 * Remember this is part of a "fake" tokenizer, that when handed a string, it simply resolves to a
 * TOKEN object matching that string. All the Tokens/Terminals Used by the parser. The purpose of
 * the enum type here is to eliminate the need for direct character comparisons.
 *
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * ----------------------------------------------------------------------------- IN *MOST* REAL
 * CASES, THERE WILL BE ONLY *ONE* LEXEME PER TOKEN! More are allowed here because this is a simple
 * English sentence parser and there are words than can be NOUNS, VERBS, etc.
 *
 * The fact that several lexemes exist per token in this example is because this is to parse simple
 * In English sentences, most of the token types have many words (lexemes) that could fit. *** This
 * is generally NOT the case in most programming languages!!! ***
 */

public enum TokenSet {
    ARTICLE("a", "the"),
    NOUN("dog", "cat", "rat", "tree", "fox", "house"),
    VERB("jumps", "chases", "climbs"),
    ADJECTIVE("fast", "slow", "lazy", "tall"),
    COMMA(","),
    ADVERB("quickly", "quietly", "slowly"),
    PUNCTUATION(".", "!"),
    PREPOSITION("around", "up", "over", "under"),
    CONJUNCTION("and", "or"),

    // The "end of input" marker.
    $$, // End of file

    // THESE ARE NOT USED IN *THIS* GRAMMAR, BUT MIGHT BE USEFUL IN ANOTHER... :)
    UNIDENTIFIED_TOKEN, // Would (usually) be an ID in a "real" programming language (HINT!!!)
    NUMBER; // A sequence of digits.

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

        // Regex for one or more digits optionally followed by and more digits.
        // (doesn't handle "-", "+" etc., only digits)
        // Return the NUMBER token if the string represents a number.
        if (lexeme.matches(LexicalAnalyzer.NUMBER_REGEX)) {
            return NUMBER;
        }

        // Search through ALL lexemes looking for a match with early bailout.
        // Return the matching token if found.
        for (var token : TokenSet.values()) {
            if (token.lexemeList.contains(lexeme)) {
                // early bailout from the method within the loop.
                return token;
            }
        }

        // NOTE: UNIDENTIFIED_TOKEN could represent an ID, for example.
        // Return "UNIDENTIFIED_TOKEN" if no match was found.
        return UNIDENTIFIED_TOKEN;
    }
}
