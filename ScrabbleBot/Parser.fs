module Parser

open Eval
open ScrabbleUtil
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.
open StateMonad


type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>
type square = Map<int, squareFun>

type boardFun2 = coord -> Result<square option, Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

let pIntToChar = pstring "intToChar"
let pCharToInt = pstring "charToInt"
let pPointValue = pstring "pointValue"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

//7.2
//The parser SATISFY : (char -> bool) -> Parser<char> takes a function f and returns a parser p
//that successfully parses any character c such that f c = true .

//The parser MANY : Parser<'a> -> Parser<'a list> takes a parser p and returns a parser that
//parses 0 or more occurrences of whatever p parses and puts the result in a list.
//Like regex *.

//The parser MANY1 : Parser<'a> -> Parser<'a list>
//exactly like many except it requires p to be able to be parsed at least once.
//Like regex +.

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"
let palphanumeric = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

let spaces = many whitespaceChar
let spaces1 = many1 whitespaceChar <?> "space1"

//7.3
(*
        
        (.>>.) : Parser<'a> -> Parser<'b> -> Parser<'a * 'b> 
        that given two parsers p1 and p2 returns a parser that first parses p1 and subsequently parses p2 and puts their respective results in a tuple.
        
        (.>>) : Parser<'a> -> Parser<'b> -> Parser<'a> 
        which is similar to .>>. but where the result from the trailing parser is discarded.
        
        (>>.) : Parser<'a> -> Parser<'b> -> Parser<'b> 
        which is similar to .>>. but where the result from the leading parser is discarded.
        
        *)

// parser1 .>> spaces - means we parse parser1 keep it and then spaces and discards it.
// parser1 .>> spaces .>>. parser2 means that spaces are now gone, so .>>. keeps the result from parser1 and parser2.
//The dot points in the direction of the data we are keeping.
let (.>*>.) parser1 parser2 = parser1 .>> spaces .>>. parser2

let (.>*>) parser1 parser2 = parser1 .>> spaces .>> parser2 //Keep left side .>> is to keep left side.
let (>*>.) parser1 parser2 = parser1 >>. spaces >>. parser2 //Keep right side >>. is to keep right side.


//7.4
//Create a parser parenthesise : Parser<'a> -> Parser<'a> that given a parser p returns a parser that
//parses a string of the form (<arbitrary many spaces><whatever p parses><arbitrary many spaces>)

//>*>. keeps result to the right, and trims whitespace.
//.>*> keeps result to the left, and trims whitespace.
let parenthesise parser = pchar '(' >*>. parser .>*> pchar ')'

//Hint: It may be beneficial to create a similar function that uses { and } in stead of parentheses. Such a
//function will come in handy when we parse statements like while-loops or if-statements.
let braces parser = pchar '{' >*>. parser .>*> pchar '}'

//7.5
//An identifier starts with a character in the alphabet, or an underscore '_', followed by a
//(possibly empty) sequence of alphanumeric characters (letters and numbers) or underscores.

//(|>>) : Parser<'a> -> ('a ->' b) -> Parser<'b>
//takes a parser p and a function f and returns a parser that parses the same input as p but which maps the result using f

//<|> : Parser<'a> -> Parer<'a> -> Parser<'a>
//that given two parser combinators p1 and p2 parses either p1 or p2 ( p2 only if p1 fails)

let pid =
    let convertListOfCharsToString (list: char list) = System.String.Concat list
    //Take first char or underscore, keep it, then take as many of alphanumeric characters or underscores.
    (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_')
    // returns a parser that parses the same input as above but which maps the result using f
    |>> fun (char, charList) -> char :: charList |> convertListOfCharsToString


//7.6
//Do operation, and discard result. Only keep result from parser.
let unop operation parser = operation >*>. parser

//7.7
//Only keep result for Parser1 and parser2
let binop operation parser1 parser2 = parser1 .>*> operation .>*>. parser2

//7.8
(*
        Term ::= Prod + Term
            | Prod
        
        Prod ::= Atom * Prod
            | Atom
        
        Atom ::= n
            | ( Term )
        
        A ::= A + A
        | A - A Sub
        | A * A Mul
        | A / A Div
        | A % A Mod 
        | pointValue ( A ) PV
        | charToInt ( C )
        | -A  Neg
        | n integer literas,
        | v variable
        | ( A ) parenthesised expression
        
        *)

//Bind hierarchy
//1. Addition and subtraction
//2. Multiplication, division, and modulo
//3. negation, point value, variables, and integer literals

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()

//Must be here to handle mutual recursion of cExp and aExp
let CharParse, cref = createParserForwardedToRef<cExp> ()


//Lowest hierarchy (ie. lowest precedence) - do add/sub last.
let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

// Middle hierarchy  - Do mul/div/mod before add and sub
let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]

//Highest hierarchy  - Do negation, point value, variables, and integer literals first.
let NParse = pint32 |>> N <?> "Int"
let ParParse = parenthesise TermParse
//Finally, to make your life simple, parse negation as multiplication by minus one (x = x * -1)
let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N -1, x)) <?> "Neg"
let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
let VarParse = pid |>> V <?> "Var"
let CharToIntParse = unop pCharToInt CharParse |>> CharToInt <?> "CharToInt"

do aref := choice [ CharToIntParse; NegParse; PVParse; VarParse; NParse; ParParse ]

//This produces a parser AExpParse : Parser<aExp> that parses strings into arithmetic expressions.
let AexpParse = TermParse

//7.9
(*
        C ::= 'c'
        | charValue ( A )
        | intToChar ( A )
        | toUpper ( C )
        | toLower ( C )
        *)
let ParentheisedCharParse = parenthesise CharParse
let CharacterParse = pchar ''' >>. anyChar .>*> pchar ''' |>> C <?> "C"
let CharValueParse = unop pCharValue AtomParse |>> CV <?> "CV"
let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "intToChar"
let ToLowerParse = unop pToLower CharParse |>> ToLower <?> "ToLower"
let ToUpperParse = unop pToUpper CharParse |>> ToUpper <?> "ToUpper"

do
    cref
    := choice
        [ ParentheisedCharParse
          CharacterParse
          CharValueParse
          IntToCharParse
          ToLowerParse
          ToUpperParse ]

let CexpParse = CharParse

//7.10
(*
        B ::= true
        | false
        | B /\ B
        | B \/ B
        | ~B
        | A = A
        | A <> A
        | A < A
        | A <= A
        | A > A
        | A >= A
        | isDigit ( C )
        | isLetter ( C )
        | isVowel ( C )
        | ( B )
        
        
        //Loosest to hardest / lowest to highest precedence
        1. /\ and \/
        2. = , <> , < , <= , > , and >=
        3. ~ , isLetter , isVowel , and isDigit
        
        *)

let BParseLowest, bLref = createParserForwardedToRef<bExp> ()
let BParseMiddle, bMref = createParserForwardedToRef<bExp> ()
let BParseHighest, bHref = createParserForwardedToRef<bExp> ()

//1. /\ and \/
let BAndParse = binop (pstring "/\\") BParseMiddle BParseLowest |>> Conj

let BOrParse =
    binop (pstring "\\/") BParseMiddle BParseLowest |>> (fun (b1, b2) -> b1 .||. b2)

do bLref := choice [ BAndParse; BOrParse; BParseMiddle ]
//2. = , <> , < , <= , > , and >=
let BEqualParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"

let BNotEqualParse =
    binop (pstring "<>") AexpParse AexpParse |>> (fun (a1, a2) -> a1 .<>. a2)

let BLessThanParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "ALt"

let BLessThanOrEqualToParse =
    binop (pstring "<=") AexpParse AexpParse |>> (fun (a1, a2) -> a1 .<=. a2)

let BGreaterThanParse =
    binop (pchar '>') AexpParse AexpParse |>> (fun (a1, a2) -> a1 .>. a2)

let BGreaterThanOrEqualToParse =
    binop (pstring ">=") AexpParse AexpParse |>> (fun (a1, a2) -> a1 .>=. a2)

do
    bMref
    := choice
        [ BEqualParse
          BNotEqualParse
          BLessThanParse
          BLessThanOrEqualToParse
          BGreaterThanParse
          BGreaterThanOrEqualToParse
          BParseHighest ]
//3. ~ , isLetter , isVowel , and isDigit
let ParentheisedBParse = parenthesise BParseLowest //Ændret fra lowest til highest
let BNotParse = unop (pchar '~') BParseLowest |>> Not
let BIsLetterParse = unop pIsLetter CexpParse |>> IsLetter <?> "IsLetter"
let BIsVowelParse = unop pIsVowel CexpParse |>> IsVowel <?> "IsVowel"
let BIsDigitParse = unop pIsDigit CexpParse |>> IsDigit <?> "IsDigit"
let BTrueParse = pTrue |>> (fun _ -> TT)
let BFalseParse = pFalse |>> (fun _ -> FF)

do
    bHref
    := choice
        [ BNotParse
          BIsLetterParse
          BIsVowelParse
          BIsDigitParse
          BTrueParse
          BFalseParse
          ParentheisedBParse ]

let BexpParse = BParseLowest


//7.11
(*
        S ::= v := A
        | declare v // Mandatory space between keyword and variable
        | S; S //SEQ?
        | if ( B ) then { S } else { S }
        | if ( B ) then { S }
        | while ( B ) do { S }
        *)
let StmParseLow, sLref = createParserForwardedToRef<stmnt> ()
let StmParseHigh, sHref = createParserForwardedToRef<stmnt> ()

let SSeqParse = binop (pchar ';') StmParseHigh StmParseLow |>> Seq <?> "Seq"
do sLref := choice [ SSeqParse; StmParseHigh ]

let StmntBracesParse = braces StmParseHigh
let SAssignParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
let SDeclareParse = pdeclare .>> spaces1 >>. pid |>> Declare <?> "Declare"

let SIfThenElseParse =
    //If content is in braces is true, then process first if {} braces, else process second else {} braces.
    pif >*>. ParentheisedBParse .>*> pthen .>*>. StmntBracesParse .>*> pelse
    .>*>. StmntBracesParse
    |>> (fun ((b, stm1), stm2) -> ITE(b, stm1, stm2))
    <?> "ITE"

let SIfThenParse =
    //If content is in braces is true, then process first if {} braces, else skip.
    pif >*>. ParentheisedBParse .>*> pthen .>*>. StmntBracesParse
    |>> (fun (bool, statement) -> ITE(bool, statement, Skip))

let SWhileParse =
    pwhile >*>. ParentheisedBParse .>*> pdo .>*>. StmntBracesParse
    |>> (fun (bool, statement) -> While(bool, statement))
    <?> "While"

do
    sHref
    := choice
        [ StmntBracesParse
          SAssignParse
          SDeclareParse
          SIfThenElseParse
          SIfThenParse
          SWhileParse ]

let stmntParse = StmParseLow
let StmParse, sref = createParserForwardedToRef<stmnt> ()
let SseqParse, seqref = createParserForwardedToRef<stmnt> ()

let parseSquareProg (squareProgram: squareProg) =
    squareProgram
    //Sort by priority
    |> Map.toList
    |> List.sortBy fst
    |> Map.ofList
    //Parse each statement
    |> Map.map (fun _ (statementAsString: string) ->
        statementAsString |> run stmntParse |> getSuccess |> stmntToSquareFun)

let parseBoardProg (source: string) sqs =
    let stmnt = run stmntParse source |> getSuccess
    stmntToBoardFun stmnt sqs

let mkBoard (bp: boardProg) : board =
    { center = bp.center
      defaultSquare = bp.squares |> Map.find bp.usedSquare |> parseSquareProg
      squares =
        bp.squares
        //sort by priority
        |> Map.toList
        |> List.sortBy fst
        |> Map.ofList
        //Parse every square
        |> Map.map (fun priority squareSourceCode -> parseSquareProg squareSourceCode)
        |> parseBoardProg bp.prog }
