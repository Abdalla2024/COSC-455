(*************************************************************************************    
    !!!!IMPORTANT!!!!
    This is *NOT* a full implementation or even the same grammar as in the assignment. 
    
    IT IS FOR CONCEPT ILLUSTRATION PURPOSES ONLY.
    THE CODE WILL NEED TO BE MODIFIED TO ACCOMMODATE THE ASSIGNMENT.

    !!!!IMPORTANT!!!!     
**************************************************************************************)


// Simple Sample Grammar: 
// 
//    <program>    ->  <stmt_list> $$
//    <stmt_list>  ->  <stmt> <stmt_list> | ε
//    <stmt>       ->  id ':=' <expr> | 'write' <expr> | <for_stmt>
//    <term_tail>  ->  <AddOp> <term> <term_tail> | ε
//    <expr>       ->  <term> <term_tail>
//    <for_stmt>   ->  'for' id '=' id 'to' id <step_stmt> 'do' <stmt_list> 'done'
//    <step_stmt>  ->  'step' id | ε
//    <term>       ->  id 
//    <AddOp>      ->  '+' | '-'


// Tokens
type Token =
    | WRITE  // the "write" keyword
    | ASSIGN   // the assignment operator "<-"
    | EQUAL  // the assignment operator "="
    | FOR    // the "for" keyword
    | TO     // the "to" keyword
    | DO     // the "do" keyword
    | DONE   // the "done" keyword
    | ADDOP  // addition or subtraction operators "+" or "-"
    | CONDOP // conditional operators "<", "==", etc.
    | STEP   // the "step" keyword
    | ID of string // ID may be any token/string not above/below, but ID is also bound to a string value
    | READ    // the "read" keyword
    | IF      // the "if" keyword
    | THEN    // the "then" keyword
    | ELSE    // the "else" keyword
    | ENDIF   // the "endif" keyword
    | IN      // the "in" keyword
    | WHILE   // the "while" keyword
    | OD      // the "od" keyword
    | DEF     // the "def" keyword
    | ARRAY_START  // the array start operator "["
    | ARRAY_END    // the array end operator "]"
    | PAREN_OPEN   // the parenthesis open operator "("
    | PAREN_CLOSE  // the parenthesis close operator ")"
    | COMMA        // the comma operator ","
    | ARITH_OP of string  // arithmetic operators "+", "-", "*", "/"
    | REL_OP of string    // relational operators ">", "<", "=="

    // Member Function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        | "read" -> READ
        | "write" -> WRITE
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "endif" -> ENDIF
        | "for" -> FOR
        | "in" -> IN
        | "while" -> WHILE
        | "do" -> DO
        | "od" -> OD
        | "def" -> DEF
        | "<-" -> ASSIGN
        | "=" -> EQUAL
        | "[" -> ARRAY_START
        | "]" -> ARRAY_END
        | "(" -> PAREN_OPEN
        | ")" -> PAREN_CLOSE
        | "," -> COMMA
        | "+" | "-" | "*" | "/" -> ARITH_OP str
        | ">" | "<" | "==" -> REL_OP str
        | _ -> ID str



/// NOTE: The `and` keyword in F# is used to define mutually recursive functions or types.
/// It allows functions to reference other functions which have not been defined.
/// 
/// Classic Example:
///   ```fsharp code
///   let rec isEven x =
///       if x = 0 then true
///       else isOdd (x - 1)
///   and isOdd x =
///       if x = 0 then false
///       else isEven (x - 1)


// Start parsing by piping the list to the first rule
// TIP: to test a specific rule, just pipe the list to that rule instead of "program"
let rec start xs = xs |> program

// <program> ::= <stmt_list> $$
and program xs =
    // Note that the "$$" is a special meta-token that represents the "Top of Stack" or "Empty List"
    // F#/OCaml allow naming functions with symbols by delimiting with double "backticks" (grave accent marks) as shown below.
    xs |> stmt_list |> ``$$``

    // NOTE: Your code should diplay a sucess or failure message within this function.


// As "$$" means "Top of Stack", this represents an "Empty List"
// Functions can be named virtually anything if in backticks.  This is a good way to name a function with a symbol.
and ``$$`` =
    function
    | [] -> printfn "The input follows the grammar."
    | remaining -> failwithf "Unexpected tokens remaining: %A" remaining


// <stmt_list> ::= <stmt> <stmt_list> | ε
// stmt_list may be empty/epsilon, so no fail case is necessary. (if an epsilon production, the list will be returned unchanged)
and stmt_list lst =
    // Since FIRST(stmt_list) is { WRITE, FOR, ID, then it is a stmt, otherwise, it is an epsilon production.
    // "_" is a wildcard, so it will match any token. We don't care what it is, just that it is there.
    match lst with
    | (ID _ | READ | WRITE | IF | FOR | WHILE | DEF) :: _ -> lst |> stmt |> stmt_list
    | x -> x // ε case, so just return the list unchanged.


// <stmt> ::= id assign | read_stmt | write_stmt | if_stmt | for_stmt | while_stmt | fun_def
and stmt lst =
    // For debugging!
    printfn $"DEBUG stmt rule: The token list  = %A{lst}" // the is a format specifier will print the whole list

    match lst with
    | ID _ :: (ASSIGN | EQUAL) :: _ -> lst |> assign
    | READ :: _ -> lst |> read_stmt
    | WRITE :: _ -> lst |> write_stmt
    | IF :: _ -> lst |> if_stmt
    | FOR :: _ -> lst |> for_stmt
    | WHILE :: _ -> lst |> while_stmt
    | DEF :: _ -> lst |> fun_def
    | _ -> failwithf "Invalid statement: %A" lst


// <expr> ::= <term> <term_tail>
and expr lst =
    // Nothing to check, so just follow and implement the rule as is.
    lst |> term |> term_tail


// <term_tail> ::=  <AddOp> <term> <term_tail> | ε
and term_tail =
    function
    | ARITH_OP _ :: xs -> xs |> term |> term_tail
    | x -> x // ε case, so just return the list unchanged.


// <term> ::= id
and term =
    function
    | ID _ :: xs -> xs
    | _ -> failwithf "Expecting an ID"



// <for_stmt> ::= for id in for_tail
and for_stmt lst =
    match lst with
    | FOR :: ID _ :: IN :: xs -> xs |> for_tail
    | _ -> failwith "Invalid for statement"


// <for_tail> ::= array stmt_block | id stmt_block
and for_tail lst =
    match lst with
    | ARRAY_START :: _ -> lst |> array |> stmt_block
    | ID _ :: _ -> lst |> stmt_block
    | _ -> failwith "Invalid for tail"


// <stmt_block> ::= do stmt_list od
and stmt_block lst =
    match lst with
    | DO :: xs -> xs |> stmt_list |> function
        | OD :: rest -> rest
        | _ -> failwith "Expected 'od'"
    | _ -> failwith "Expected 'do'"


// <read_stmt> ::= read id
and read_stmt lst =
    match lst with
    | READ :: ID _ :: xs -> xs
    | _ -> failwith "Invalid read statement"


// <write_stmt> ::= write expr
and write_stmt lst =
    match lst with
    | WRITE :: xs -> xs |> expr
    | _ -> failwith "Invalid write statement"


// <if_stmt> ::= if cond then stmt_list else_stmt
and if_stmt lst =
    match lst with
    | IF :: xs -> xs |> cond |> function
        | THEN :: ys -> ys |> stmt_list |> else_stmt
        | _ -> failwith "Expected 'then'"
    | _ -> failwith "Invalid if statement"


// <else_stmt> ::= else stmt_list endif | endif
and else_stmt lst =
    match lst with
    | ELSE :: xs -> xs |> stmt_list |> function
        | ENDIF :: ys -> ys
        | _ -> failwith "Expected 'endif'"
    | ENDIF :: xs -> xs
    | _ -> failwith "Expected 'else' or 'endif'"


// <fun_def> ::= def id ( id_list ) stmt_block
and fun_def lst =
    match lst with
    | DEF :: ID _ :: PAREN_OPEN :: xs -> xs |> id_list |> function
        | PAREN_CLOSE :: ys -> ys |> stmt_block
        | _ -> failwith "Expected ')'"
    | _ -> failwith "Invalid function definition"


// <while_stmt> ::= while cond stmt_block
and while_stmt lst =
    match lst with
    | WHILE :: xs -> xs |> cond |> stmt_block
    | _ -> failwith "Invalid while statement"


// <assign> ::= <- fun_call | = assign_tail
and assign lst =
    match lst with
    | ID _ :: ASSIGN :: xs -> xs |> fun_call
    | ID _ :: EQUAL :: xs -> xs |> assign_tail
    | _ -> failwith "Invalid assignment"


// <assign_tail> ::= expr | array
and assign_tail lst =
    match lst with
    | ARRAY_START :: _ -> lst |> array
    | _ -> lst |> expr


// <fun_call> ::= id ( param_list )
and fun_call lst =
    match lst with
    | ID _ :: PAREN_OPEN :: xs -> xs |> param_list |> function
        | PAREN_CLOSE :: ys -> ys
        | _ -> failwith "Expected ')'"
    | _ -> failwith "Invalid function call"


// <array> ::= [ id_list ]
and array lst =
    match lst with
    | ARRAY_START :: xs -> xs |> id_list |> function
        | ARRAY_END :: ys -> ys
        | _ -> failwith "Expected ']'"
    | _ -> failwith "Invalid array"


// <param_list> ::= expr param_tail | ε
and param_list lst =
    match lst with
    | (ID _ | PAREN_OPEN) :: _ -> lst |> expr |> param_tail
    | _ -> lst


// <param_tail> ::= , expr param_tail | ε
and param_tail lst =
    match lst with
    | COMMA :: xs -> xs |> expr |> param_tail
    | _ -> lst


// <id_list> ::= id id_tail
and id_list lst =
    match lst with
    | ID _ :: xs -> xs |> id_tail
    | _ -> failwith "Expected identifier"


// <id_tail> ::= , id_list | ε
and id_tail lst =
    match lst with
    | COMMA :: xs -> xs |> id_list
    | _ -> lst


// <cond> ::= expr rel_oper expr
and cond lst =
    lst |> expr |> function
    | REL_OP _ :: xs -> xs |> expr
    | _ -> failwith "Invalid condition"


(* ************************************************************************************************ *
 * !!!!!!!!!!!!!!!!!!!!!!!!! DO NOT MODIFY ANY CODE BELOW THIS POINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
 * ************************************************************************************************ *)

// Import the regex module
open System.Text.RegularExpressions

// NOTE: To make the 'let' assignment be a function that accepts 0 parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#. (In the ML family, there 
// is no such thing as a function which does not accept a parameter.)
let main () =
    // A simple 'helper' to help debug the mapping process.
    let displayMapping str = printfn $"Mapping \"%s{str}\" -> %O{Token.tokenFromLexeme str}"; str

    // Convert a list of stings to Tokens:
    // This is very ".NET" specific. Regex.Matches is part of the (Microsoft).NET API.        
    let getTokenList str = 
        let tokenRegex = 
            @"(?x)" +  // "@" marks a regex string, (?x) is "PCRE style" Extended mode (ignore whitespace) 
            "read|write|if|then|else|endif|for|in|while|do|od|def|" +
            "<-|=|\[|\]|\(|\)|,|\+|-|\*|/|>|<|==|" +
            "\d+|\w+|[^\s]"
            
        // The regex matches the following tokens:
        Regex.Matches (str, tokenRegex) |>
        Seq.toList |>
        List.map string |>
        List.map _.ToLower() |>  // Yes, a violation of not using "methods", but much simpler in this case.
        List.map (displayMapping >> Token.tokenFromLexeme)


    // Begin the Parsing Process
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"

        // Try to parse the list of tokens and display the results.
        try
            let tokenList = getTokenList str
            printfn $"\nToken List BEFORE Parsing: %A{tokenList}"

            let parsedList = start tokenList
            printfn $"Tokens List AFTER Parsing: %O{parsedList}"

        // If an exception ("failwith") is thrown, display the error message.
        with Failure msg ->
            printfn $"Error: %s{msg}"


    // Get the user input and start parsing
    let getUserInput () =
        printf "Input String\n=> "    
        System.Console.ReadLine()  // Note that console I/O uses the .NET library in F#.  
                                   // For OCaml, See: https://ocaml.org/manual/5.2/api/Stdlib.html#VALread_line

    // Get the user input and start parsing
    in getUserInput () |> startParsing |> ignore  // Just ignore the result, as we are just printing results above.


// Uncomment the following lines to run the program in continuous a loop.
// This will allow you to enter multiple strings without restarting the program.
// *** IF YOU UNCOMMENT THIS FOR TESTING, CHANGE IT BACK BEFORE SUBMITTING!!! ***
// let rec run () : unit = main (); run () 
// run ()  

// Run the program once
main ()