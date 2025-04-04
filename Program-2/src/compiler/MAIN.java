//  ********* REQUIRES Java 21 OR ABOVE! (https://adoptium.net/) *********
package compiler;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static java.lang.System.out;
import static java.nio.file.Files.lines;
import static java.text.MessageFormat.format;

/*
 * COSC455 Programming Languages: Implementation and Design.
 *
 * ************** NOTE: REQUIRES Java 21 OR ABOVE! (https://adoptium.net/) ******************
 *
 * DESIGN NOTE: It is generally bad to have a bunch of "top level classes" in one giant
 * file. However, this was done here only to keep the example down to only a few files.
 *
 * This syntax analyzer implements a top-down, left-to-right,
 * recursive-descent parser based on the production rules for a simple
 * English language provided by Weber in "Modern Programming Languages".
 */
public class MAIN {
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // !!!!!!!!!!!!!!!!!! Global Flags !!!!!!!!!!!!!!!!!!!!
    static final boolean DEBUG_LEXER = true;

    // The following can all be overridden in the Parser class.
    public static boolean PROMPT_FOR_GRAPHVIZ = true;
    public static boolean WRITE_TO_DOT_FILE = true;
    public static String DOTFILE_EXT = ".gv";
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    /**
     * Private constructor to prevent instantiation.
     * This is purely a utility class.
     */
    private MAIN() {
        // Private constructor
    }

    /**
     * The main entry point for the program.
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args) {
        // Set the look and feel to the system L&F.
        setLaF();

        // The input file name.
        File inputFile;

        // Get the input file path.
        final Optional<File> optInputFile = getInputFilePath(args);

        // Bailout if no file was chosen.
        if (optInputFile.isEmpty())
            return;
        else
            inputFile = optInputFile.get();

        // Try to compile the input file.
        try {
            // Scan and parse the input file.
            final String compiledCode = scanAndParse(inputFile);

            // Write the output to a file.
            if (WRITE_TO_DOT_FILE)
                writeToDotFile(inputFile, compiledCode);

            // Display the graphviz test page, if desired.
            if (PROMPT_FOR_GRAPHVIZ) {
                GraphViewer gv = new GraphViewer();
                gv.displayTreeViewer(compiledCode, PROMPT_FOR_GRAPHVIZ);
            }

        } catch (IOException ex) {
            final String msg = format("Error reading the file!!! {0}", ex.getMessage());
            Logger.getGlobal().log(Level.SEVERE, msg);
        }
    }

    /**
     * Set the look and feel...
     */
    static void setLaF() {
        // Set the look and feel to the system L&F.
        if (Desktop.isDesktopSupported()) {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
                    | UnsupportedLookAndFeelException e) {
                System.err.println("Could not set the Look and Feel to the system default.");
            }
        } else {
            out.println("No GUI support.");
        }
    }

    /**
     * Get the input file path.
     *
     * @param args The command line arguments.
     * @return The input file path.
     */
    private static Optional<File> getInputFilePath(String[] args) {
        // The input file name.
        String fileName;

        // Check for an input file argument
        if (args.length < 1) {
            System.err.println("No input filename provided!!!");
            System.err.println("Prompting for file selection instead...");

            var optFileName = getFileNameFromFileChooser();

            if (optFileName.isEmpty())
                return Optional.empty();
            else
                fileName = optFileName.get();

        } else {
            fileName = args[0];
        }

        final File inputFile = new File(fileName);

        // Check if the file exists and is readable.
        if (!inputFile.exists() || !inputFile.isFile() || !inputFile.canRead()) {
            System.err.printf("Input file not found/readable: %s%n", inputFile.toPath());
            return Optional.empty();
        }

        return Optional.of(inputFile);
    }

    /**
     * Scan and parse the input file.
     *
     * @param inputFile The input file to be scanned and parsed.
     * @return The compiled code.
     * @throws IOException If the file cannot be read.
     */
    private static String scanAndParse(final File inputFile) throws IOException {
        // Create the code generator and lexical analyzer.
        final CodeGenerator codeGenerator = new CodeGenerator();
        final LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();

        // Init the lexer
        lexicalAnalyzer.buildTokenQueue(inputFile.toPath());

        // Compile the program from the input supplied by the lexical analyzer.
        final Parser parser = new Parser(lexicalAnalyzer, codeGenerator);

        // Generate a header for the output.
        if (!WRITE_TO_DOT_FILE)
            out.println("\n// START: Graphviz Parse Tree Output");
        TreeNode startNode = codeGenerator.writeHeader();

        // Analyze the input and generate the parse tree.
        parser.analyze(startNode);

        // generate footer for our output
        codeGenerator.writeFooter();
        if (!WRITE_TO_DOT_FILE)
            out.println("// END: Graphviz Parse Tree Output\n");

        // Return the compiled code.
        return codeGenerator.getGeneratedCodeBuffer();
    }

    /**
     * Write the compiled code to a file.
     *
     * @param inputFile    The input file (for obtaining the canonical path).
     * @param compiledCode The output code to be written to the file.
     * @throws IOException Throws an IOException if the file cannot be written.
     */
    private static void writeToDotFile(File inputFile, String compiledCode) throws IOException {
        // Get the canonical path of the input file and change the extension to .gv (or
        // .dot)
        String canonicalPath = inputFile.getCanonicalPath().replaceFirst("[.][^.]+$", DOTFILE_EXT);

        try (var outputFile = new FileWriter(canonicalPath)) {
            out.printf("Writing output to: %s%n", canonicalPath);
            outputFile.write(compiledCode);
        } catch (IOException ex) {
            final String msg = format("Error writing the file!!! {0}", ex.getMessage());
            System.err.println(msg);
        }
    }

    /**
     * Get the filename from the file chooser.
     * <p>
     * This is just a quick hack, as it will only be when the program is not invoked
     * properly anyhow.
     *
     * @return The filename from the file chooser.
     */
    private static Optional<String> getFileNameFromFileChooser() {
        if (Desktop.isDesktopSupported()) {
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));

            if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                return Optional.of(fileChooser.getSelectedFile().getAbsolutePath());
            } else {
                return Optional.empty();
            }
        } else {
            return Optional.empty();
        }
    }
}

// *********************************************************************************************************

/**
 * This is a *FAKE* (Overly simplified) Lexical Analyzer...
 * <p>
 * NOTE: This DOES NOT "lex" the input in the traditional manner on a DFA based
 * "state machine".
 * <p>
 * Instead of using "state transitions", this is merely a quick hack to create
 * something that BEHAVES like a traditional lexer in its FUNCTIONALITY, but it
 * ONLY knows how to separate (tokenize) lexemes matching a simple regular
 * expression. A Real TokenSet would tokenize based upon far more sophisticated
 * lexical rules/expressions.
 */
class LexicalAnalyzer {
    public static final String NUMBER_REGEX = "(\\+|-)?\\d+(?:\\.\\d+)?";
    public static final String ID_REGEX = "[a-zA-Z]+\\w*";
    public static final String SINGLE_CHAR_OPERATOR_REGEX = "\\p{Punct}";

    public static final String MULTICHAR_OPERATOR_REGEX =
            // Comparison operators
            ">=|<=|=>|=<|<>|==|!=" +
            // Bitwise and shift operators
                    "|>>|<<|\\|=|&=|\\^=" +
                    // Assignment operators
                    "|:=|\\+=|-=|\\*=|/=|%=|&&|\\|\\|" +
                    // Increment/Decrement operators
                    "|\\+\\+|--" +
                    // Arrow operators
                    "|<-|->";

    // TokenSet Regex for words, numbers, and symbols.
    private final static String regex = String.join("|", NUMBER_REGEX, ID_REGEX, MULTICHAR_OPERATOR_REGEX,
            SINGLE_CHAR_OPERATOR_REGEX);

    // The Compiled pattern.
    private final Pattern pattern;

    // TOKENIZED input.
    private Queue<TokenString> tokenList;

    /**
     * Construct the TokenSet.
     * <p>
     * buildTokenQueue must be invoked to "prime" the TokenSet.
     */
    LexicalAnalyzer() {
        this.pattern = Pattern.compile(regex);
    }

    /**
     * Read the file and tokenize the input.
     *
     * @param filePath the path to the file to be tokenized.
     * @throws IOException if the file cannot be read.
     */
    void buildTokenQueue(Path filePath) throws IOException {
        // Read the file and tokenize the input.
        try (var lines = lines(filePath)) {
            this.tokenize(lines // read lines
                    .map(String::trim) // map to stripped strings
                    .filter(x -> !x.startsWith("#")) // filter out lines starting with #
                    .collect(Collectors.joining(" "))); // join lines together with spaces between.
        }
    }

    /**
     * Tokenize the input string.
     *
     * @param line the input string to be tokenized.
     */
    private void tokenize(final CharSequence line) {
        // Create a matcher for the input string.
        final Matcher matcher = pattern.matcher(line);

        // Add all matches to the token list.
        this.tokenList = new LinkedList<>();

        // Add all matches to the token list.
        while (matcher.find()) {
            // Create a new token from the match.
            var lexeme = matcher.group();
            var token = TokenSet.getTokenFromLexeme(lexeme);
            TokenString tokenLexemePair = new TokenString(lexeme, token);

            // Debugging output only
            if (MAIN.DEBUG_LEXER)
                out.println("READING: " + tokenLexemePair);

            // Add the token to the list.
            this.tokenList.add(tokenLexemePair);
        }
    }

    /**
     * Get the current lexeme from the head of the token list.
     *
     * @return the current lexeme.
     */
    public String getCurrentLexeme() {
        return (this.tokenList.isEmpty() || getCurrentToken() == TokenSet.$$)
                ? "$$ (End of Input)"
                : Objects.requireNonNull(this.tokenList.peek()).lexeme;
    }

    /**
     * Get the current token type from the head of the token list.
     *
     * @return the current token type.
     */
    public TokenSet getCurrentToken() {
        return this.tokenList.isEmpty() ? TokenSet.$$ : this.tokenList.peek().token;
    }

    /**
     * Get the current token type from the head of the token list.
     */
    public void advanceToken() {
        if (!this.tokenList.isEmpty()) {
            this.tokenList.remove();
        }
    }

    @Override
    public String toString() {
        return this.tokenList.toString();
    }

    /**
     * Nested class: a "Pair Tuple/Struct" for the token type and original string.
     */
    private record TokenString(String lexeme, TokenSet token) {
        @Override
        public String toString() {
            return String.format("{lexeme='%s', token=%s}", lexeme, token);
        }
    }
}

// *********************************************************************************************************
// Rev Hash: XgGccfVNFdMj
// *********************************************************************************************************

/**
 * This is a ***SIMULATION*** of a "code generator" that simply generates
 * GraphViz output.
 * Technically, this would represent the "Intermediate Code Generation" step.
 * <p>
 * Also, Instead of building an entire tree in memory followed by a traversal
 * tree at the end,
 * here we are just adding “code” as we go.
 * <p>
 * (This simulates a single-pass compiler; keep in mind that most modern
 * compilers work in several
 * passes… eg. Scan for all top level identifiers, build subtrees for each
 * class/method/etc.,
 * generate an internal intermediate code representation, and so on).
 * <p>
 * DESIGN NOTE: From an OOP design perspective, creating instances of "utility
 * classes" (classes
 * with no internal state) is generally bad. However, in a more elaborate
 * example, the code
 * generator would most certainly maintain some internal state information.
 * (Memory address offsets, etc.)
 */
class CodeGenerator {
    private static final String GRAPHVIZ_ROOT_STYLE = "shape=plain";
    private static final String GRAPHVIZ_TERMINAL_STYLE = "shape=oval,style=bold";
    private static final String GRAPHVIZ_NON_TERMINAL_STYLE = "shape=rect,style=dotted";
    private static final String GRAPHVIZ_EPSILON_STYLE = "shape=plain";
    private static final String GRAPHVIZ_SYNTAX_ERROR = "shape=plain,fontcolor=red";
    private static final Integer RANK_SEP = 1;

    // Buffer for generated code
    private final StringBuffer codeBuffer;

    // Constructor
    CodeGenerator() {
        this.codeBuffer = new StringBuffer();
    }

    /**
     * Write the header for the "compiled" output.
     *
     * @return The newly created node.
     */
    TreeNode writeHeader() {
        // The header for the "compiled" output
        var headerNode = this.buildNode("Parse Tree");

        String sb = """
                digraph ParseTree {
                graph[ranksep=%d];
                node[%s];

                %s[label="%s",%s];%n""".formatted(
                RANK_SEP,
                GRAPHVIZ_NON_TERMINAL_STYLE,
                headerNode.id(),
                headerNode.name(),
                GRAPHVIZ_ROOT_STYLE);

        this.outputGeneratedCode(sb);
        return headerNode;
    }

    // Build a node name, so it can be later "deconstructed" for the output.
    private TreeNode buildNode(final String name) {
        return new TreeNode(name);
    }

    // Write generated code to both the screen AND the buffer.
    private void outputGeneratedCode(final String outputAsSting) {
        if (!MAIN.WRITE_TO_DOT_FILE)
            out.print(outputAsSting);
        this.codeBuffer.append(outputAsSting);
    }

    /**
     * Add a terminal node to the parse tree.
     *
     * @param parentNode    The parent of the terminal node.
     * @param currentToken  The token to be added.
     * @param currentLexeme The lexeme of the token beign added.
     */
    TreeNode addTerminal(final TreeNode parentNode, final TokenSet currentToken, final String currentLexeme) {
        var nodeLabel = "<%s>".formatted(currentToken);
        var terminalNode = addNonTerminal(parentNode, nodeLabel);
        return addTerminal(terminalNode, currentLexeme);
    }

    /**
     * Add the "from node" to the tree and return a new "next node" object.
     *
     * @param parentNode The node to add to the tree.
     * @return the newly added node as ParseNode object.
     */
    TreeNode addNonTerminal(final TreeNode parentNode, final String toNodeString) {
        final var childNode = this.buildNode(toNodeString);
        return addNonTerminal(parentNode, childNode);
    }

    /**
     * Add a terminal node to the parse tree.
     *
     * @param parentNode The parent of the terminal node.
     * @param lexeme     The lexeme of the token to be added.
     */
    private TreeNode addTerminal(final TreeNode parentNode, final String lexeme) {
        final var childNode = new TreeNode(lexeme);
        String template = "%s->{%s[label=\"%s\",%s]};%n";
        final var msg = String.format(template, parentNode.id(), childNode.id(), lexeme, GRAPHVIZ_TERMINAL_STYLE);

        this.outputGeneratedCode(msg);

        return childNode;
    }

    /**
     * Show the non-terminals as boxes…
     *
     * @param parentNode the parent node
     * @param childNode  the child node
     * @return the child node
     */
    private TreeNode addNonTerminal(final TreeNode parentNode, final TreeNode childNode) {
        final var msg = "%s->{%s[label=\"%s\"]};%n"
                .formatted(parentNode.id(), childNode.id(), childNode.name());

        this.outputGeneratedCode(msg);
        return childNode;
    }

    /**
     * Add an EMPTY terminal node (result of an Epsilon Production) to the parse
     * tree.
     * Mainly, this is just done for better visualizing the complete parse tree.
     *
     * @param parentNode The parent of the terminal node.
     */
    void addEpsilon(final TreeNode parentNode) {
        final var newNode = new TreeNode("EMPTY");
        String template = "%s->{%s[label=\"%s\",%s]};%n";
        final var msg = String.format(template, parentNode.id(), newNode.id(), "&epsilon;", GRAPHVIZ_EPSILON_STYLE);

        this.outputGeneratedCode(msg);
    }

    /**
     * Output the error message and throw a ParseException.
     *
     * @param parentNode The node from which the error occurred.
     * @param errMsg     The error message to be displayed.
     * @throws ParseException Thrown with the error message.
     */
    void syntaxError(TreeNode parentNode, final String errMsg) throws ParseException {
        String template = "%s->{\"%s\"[%s]};%n";
        var msg = String.format(template, parentNode.id(), errMsg, GRAPHVIZ_SYNTAX_ERROR);

        this.outputGeneratedCode(msg);
        throw new ParseException(errMsg);
    }

    /**
     * Write the footer for the "compiled" output.
     * "Real" executable code generally has a footer.
     * See: <a href=
     * "https://en.wikipedia.org/wiki/Executable_and_Linkable_Format">...</a>
     */
    void writeFooter() {
        this.outputGeneratedCode("}");
    }

    /**
     * Get the generated code buffer.
     *
     * @return The generated code buffer.
     */
    String getGeneratedCodeBuffer() {
        return codeBuffer.toString();
    }
}

// *********************************************************************************************************

/**
 * A "3-Tuple" for the node name and ID number.
 */
class TreeNode {

    private static Integer runningNodeID = 0;
    private final String nodeName;
    private final Integer nodeId;

    TreeNode(final String nodeName) {
        this.nodeName = nodeName;

        // Note that assignment to static fields is generally a bad practice, but it is
        // done here only for simplicity.
        this.nodeId = TreeNode.runningNodeID++;
    }

    /**
     * Get the node name.
     *
     * @return The node name.
     */
    String name() {
        return nodeName;
    }

    /**
     * Get the node ID.
     *
     * @return The node ID.
     */
    int id() {
        return nodeId;
    }
}

/**
 * Code to invoke the online graph viewer.
 */
class GraphViewer {
    /*
     * NOTE: Online/Web versions of Graphviz
     * http://www.webgraphviz.com
     * http://viz-js.com
     * https://dreampuf.github.io/GraphvizOnline
     * https://edotor.net
     */

    static final String[] uri_fields = { "https", "edotor.net", "/" };
    // static String[] uri_fields = {"https", "dreampuf.github.io",
    // "/GraphvizOnline/"};

    static final String protocol = uri_fields[0];
    static final String host = uri_fields[1];
    static final String path = uri_fields[2];

    private static final String GRAPHVIZ_INFO = """
            To visualize your output, you may Copy/Paste the contents of the output file into:
            [%s] or any local or online Graphviz rendering tool.
            """.formatted(protocol + "://" + host + path);

    /**
     * Displays a tree viewer using the provided DOT source string and optionally
     * prompts the user
     * to open a web-based Graphviz instance.
     *
     * @param dotSrc            The DOT source string representing the tree
     *                          structure to be visualized.
     * @param promptForGraphviz A boolean flag indicating whether to prompt the user
     *                          to open
     *                          a web-based Graphviz visualization. If true, a
     *                          dialog may be presented to the user.
     */
    void displayTreeViewer(final String dotSrc, boolean promptForGraphviz) {

        // Is the Desktop supported?
        if (!Desktop.isDesktopSupported()) {
            out.println("No Desktop support for opening a browser.");
            out.println(GRAPHVIZ_INFO);
            return;
        }

        try {
            // Create the URI for the web-based Graphviz instance.
            var uri = new URI(protocol, host, path, dotSrc);

            // URI Length limit reached.
            if (uri.toString().length() >= (int) Math.pow(2, 15)) {
                out.println("Sorry, can't use remote Graphviz; Output is too long for the server.");
                out.println("To render a an image of the parse tree, use a local Graphviz installation.");
                out.println(GRAPHVIZ_INFO);
            } else if (promptForGraphviz) {
                launchWebGraphviz(uri);
            } else {
                out.println(GRAPHVIZ_INFO);
            }
        } catch (URISyntaxException e) {
            System.err.println("Invalid URI: " + e.getMessage());
        }

    }

    /**
     * Launches the web-based Graphviz interface in the default browser using the
     * specified URI.
     * If the functionality is supported and enabled, a dialog prompts the user for
     * confirmation
     * before proceeding to open the URI in a browser.
     *
     * @param uri The URI to be opened in the browser, representing the web-based
     *            Graphviz interface.
     */
    private void launchWebGraphviz(URI uri) {
        // Assume the launch is successful
        boolean successfullyLaunched = true;

        // Open the default browser with the url:
        try {
            // Get the desktop object.
            final Desktop desktop = Desktop.getDesktop();

            // Can we launch a browser?
            if (desktop.isSupported(Desktop.Action.BROWSE)) {
                String dialogMsgTmpl = """
                        <html><b>Open a web-based Graphviz instance?</b><br/>
                        (%s)<br/><br/>
                        <font size='1'>To disable this prompt, add the following in your Parser class constructor:<br/>
                        <code>MAIN.PROMPT_FOR_GRAPHVIZ = false</code></font><br/><br/>
                        </html>""";

                final var response = JOptionPane.showConfirmDialog(
                        null,
                        String.format(dialogMsgTmpl, uri.getHost()),
                        "Open Web-Based Graphviz?",
                        JOptionPane.YES_NO_OPTION);

                // Open Browser?
                if (response == JOptionPane.YES_OPTION) {
                    desktop.browse(uri);
                } else {
                    successfullyLaunched = false;
                }

            } else {
                out.println("Desktop doesn't support the browse action.");
                successfullyLaunched = false;
            }
        } catch (IOException ex) {
            System.err.println("Error launching browser: " + ex.getMessage());
            successfullyLaunched = false;
        }

        // If the launch was not successful, provide manual instructions.
        if (!successfullyLaunched) {
            out.println();
            out.println(GRAPHVIZ_INFO);
        }
    }
}

// *********************************************************************************************************

/**
 * An exception to be raised if parsing fails due to a "syntax error" in the
 * input file.
 */
final class ParseException extends RuntimeException {
    /**
     * Create a new ParseException with the given error message.
     *
     * @param errMsg The error message to be displayed.
     */
    ParseException(String errMsg) {
        super(errMsg);
    }
}
