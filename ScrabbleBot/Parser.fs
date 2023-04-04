// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

//CARSTENS FILE IKKE EGEN
module Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "True"
    let pFalse      = pstring "False"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"
    let pdeclare  = pstring "declare"
    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"

    let whitespaceChar = satisfy Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many (satisfy Char.IsWhiteSpace) <?> "space"
    let spaces1        = many1 (satisfy Char.IsWhiteSpace) <?> "space1"

    let (.>*>.) p1 p2 = p1 .>>. (spaces >>. p2)
    let (.>*>) p1 p2  = p1 .>>. (spaces >>. p2) |>> fst
    let (>*>.) p1 p2  = p1 .>>. (spaces >>. p2) |>> snd

    let parenthesise p = (pchar '(' >*>. spaces >*>. p .>*> spaces .>*> pchar ')')
    let curlyBrackets p =(pchar '{' >*>. spaces >*>. p .>*> spaces .>*> pchar '}')
    let charListToString (c:char list) = c |> List.toArray |> String
    let pid = ((pchar '_' <|> pletter) .>>. ( many palphanumeric .>>. many (pchar '_')))
            |>> fun (x, (y,z)) -> charListToString (x::y@z)
    let unop op p1 = op >*>. p1 
    let binop op p1 p2 = p1 .>*> op .>>. (spaces >>. p2)

    // refs for aexp
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    // refs for cexp    
    let cParse, cref    = createParserForwardedToRef<cExp>()
    
    // refs for bexp
    let bTermParse, btref        = createParserForwardedToRef<bExp>()
    let bProdParse, bpref = createParserForwardedToRef<bExp>()
    let bAtomParse, baref       = createParserForwardedToRef<bExp>()
    
    // refs for sexp
    let StmTermParse, stmref        = createParserForwardedToRef<stm>()
    let StmProdParse, stmpref      = createParserForwardedToRef<stm>()
    
    // aexp parsers:
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let PVParse         = pPointValue >*>. parenthesise TermParse |>> PV <?> "PV" 
    let CharIntParse    = pCharToInt >*>. parenthesise cParse |>> CharToInt <?> "CharToInt"
    let NegParse        = unop (pchar '-') TermParse |>> (fun x ->  Mul ((N -1),x)) <?> "Neg"
    let NParse          = pint32 |>> N <?> "Int"
    let VarParse        = pid |>> V <?> "string"
    let ParParse        = parenthesise TermParse
    do aref := choice [PVParse; CharIntParse; NegParse; NParse; VarParse; ParParse]

    // cexp parsers:
    let CharParse       = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let CharValParse    = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    let IntCharParse    = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    let ToUpperParse    = pToUpper >*>. parenthesise cParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse    = pToLower >*>. parenthesise cParse |>> ToLower <?> "ToLower"
    do cref := choice [CharParse; CharValParse; IntCharParse; ToUpperParse; ToLowerParse]

    // bexp parsers:
    let ConjParse       = binop (pstring "/\\" .>> many whitespaceChar) bProdParse bTermParse |>> Conj <?> "Conj"
    let DisjParse       = binop (pstring "\\/" .>> many whitespaceChar) bProdParse bTermParse |>> (fun (x, y) -> (Not (Conj (Not x, Not y)))) //<?> "Disj"

    do btref := choice [ ConjParse; DisjParse; bProdParse ]
    
    let bParParse           = parenthesise bTermParse    
    let EqualParse          = binop (pchar '=') AtomParse AtomParse |>> AEq <?> "Equal"
    let NotEqualParse       = binop (pstring "<>") AtomParse AtomParse |>> (fun x -> Not(AEq(x))) <?> "NotEqual"
    let SmallerParse        = binop (pchar '<') AtomParse AtomParse |>> ALt <?> "LessThan"
    let SmallerEqualParse   = binop (pstring "<=") AtomParse AtomParse |>> (fun x -> Not (Conj ((Not (ALt(x)), Not (Not (Not (AEq(x)))))))) <?> "LessThanEqual"
    let GreaterParse        = binop (pchar '>') AtomParse AtomParse |>> (fun x -> Conj (Not (AEq(x)), (Not (ALt(x))))) <?> "Greater"
    let GreaterEqualParse   = binop (pstring ">=") AtomParse AtomParse |>> (fun x -> Not(ALt(x)))  <?> "GreaterEqual"
    do bpref := choice [ bParParse; EqualParse; NotEqualParse; SmallerParse; SmallerEqualParse; GreaterParse; GreaterEqualParse; bAtomParse ]
    
    let NotParse            = unop (pchar '~') bTermParse |>> Not <?> "Not"
    let IsVowelParse        = pIsVowel >*>. parenthesise cParse |>> IsVowel <?> "isVowel"
    (*let isConsonantParse    = pstring "isConsonant" >*>. parenthesise cParse |>> isConsonant <?> "isConsonant"*)
    let TrueParse           = pTrue |>> (fun x -> TT) <?> "True"
    let FalseParse          = pFalse |>> (fun x -> FF) <?> "False"   
    do baref := choice [ NotParse; IsVowelParse; TrueParse; FalseParse; bProdParse; ]
    
    // stmnt parsers
    let SeqParse        = binop (pchar ';') StmProdParse StmTermParse |>> Seq <?> "seq"
    do stmref := choice [SeqParse; StmProdParse]
    let AssignParse     = binop (pstring ":=") pid TermParse |>> Ass <?> "assign"
    let DeclareParse    = pdeclare >>. whitespaceChar >*>. pid |>> Declare <?> "declare"
    let IfThenParse     = pif >*>. parenthesise bTermParse .>*> pthen .>*>. curlyBrackets StmTermParse
                        |>> (fun (b,s1) -> ITE (b, s1, Skip)) <?> "IfThen"
    let IfThenElseParse = pif >*>. parenthesise bTermParse .>*> pthen .>*>. curlyBrackets StmTermParse .>*> pelse .>*>. curlyBrackets StmTermParse
                        |>> (fun ((b,s1), s2) -> ITE (b, s1, s2)) <?> "IfThenElse"
    let WhileParse      = pwhile >*>. parenthesise bTermParse .>*> pdo .>*>. curlyBrackets StmTermParse
                        |>> (fun (b,s1) -> While (b, s1)) <?> "While"
    do stmpref := choice [ IfThenElseParse; IfThenParse; WhileParse; AssignParse; DeclareParse]
   
    
    let AexpParse = TermParse 

    let CexpParse = cParse

    let BexpParse = bTermParse
    
    let stmntParse = StmTermParse

    (* The rest of your parser goes here *)
    type word = (char * int) list
    type square = Map<int, squareFun>
    let parseSquareProg (sqp:squareProg) =
        Map.map (fun _ x -> stmntToSquareFun (getSuccess (run stmntParse x))) sqp
    type boardFun = coord -> Result<square option, Error>
    let parseBoardFun (s:string) (sqs:Map<int, Map<int, squareFun>>) : boardFun =
        stmntToBoardFun (getSuccess (run stmntParse s)) sqs
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
        isInfinite    : bool
    }
    let parseBoardProg (bp : boardProg) : board = 
        {
         center = bp.center
         defaultSquare = parseSquareProg (Map.find bp.usedSquare bp.squares)
         squares = Map.map (fun _ y -> parseSquareProg y) bp.squares |> parseBoardFun bp.prog
         isInfinite = bp.isInfinite
        }