// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.
//CARSTENS FILE - IKKE EGEN

module Eval

    open StateMonad
    
    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let add a b = 
        a >>= (fun x -> b >>= (fun y -> ret (x+y)))      
 
    let sub a b = 
        a >>= (fun x -> b >>= (fun y -> ret (x-y))) 
    
    let mul a b = 
        a >>= (fun x -> b >>= (fun y -> ret (x*y)))
        
    let div a b = 
        a >>= (fun x -> b >>= (fun y -> 
            if y <> 0 then ret (x/y)
            else fail DivisionByZero)) 
         
    let modulo a b = 
        a >>= (fun x -> b >>= (fun y -> 
            if y <> 0 then ret (x%y)
            else fail DivisionByZero))      

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       (*| isConsonant of cExp  (* check for consonant*)*)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let isVowel (c:char) =
        match c with
        |'a'|'e'|'i'|'o'|'u'|'y'|'A'|'E'|'I'|'O'|'U'|'Y' -> true
        | _ -> false
    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV a -> arithEval a >>= (fun x -> pointValue x)
        | Add (a, b) ->
            let sma = arithEval a
            let smb = arithEval b
            add sma smb
        | Sub (a, b) ->
            let sma = arithEval a
            let smb = arithEval b
            sub sma smb
        | Mul (a, b) ->
            let sma = arithEval a
            let smb = arithEval b
            mul sma smb
        | Div (a, b) ->
            let sma = arithEval a
            let smb = arithEval b
            div sma smb
        | Mod (a, b) ->
            let sma = arithEval a
            let smb = arithEval b
            modulo sma smb
        | CharToInt c -> charEval c >>= (fun x -> ret (int x))      

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= (fun x -> characterValue x)
        | ToUpper c -> charEval c >>= (fun x -> (ret (System.Char.ToUpper x)))
        | ToLower c -> charEval c >>= (fun x -> (ret (System.Char.ToLower x)))
        | IntToChar a -> arithEval a >>= (fun x -> ret (char x))   

    let rec boolEval b : SM<bool> =  
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> arithEval a >>= (fun x -> arithEval b >>= (fun y -> ret (x=y)))
        | ALt (a, b) -> arithEval a >>= (fun x -> arithEval b >>= (fun y -> ret (x<y)))
        | Not b -> boolEval b >>= (fun x -> ret (not x))
        | Conj (a, b) -> boolEval a >>= (fun x -> boolEval b >>= (fun y -> ret (x && y)))
        | IsVowel c -> charEval c >>= (fun x -> ret (isVowel x))
        //| isConsonant c -> charEval c >>= (fun x -> ret (not (isVowel x)))

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare x                 -> declare x
        | Ass (x,a)                 -> arithEval a >>= update x
        | Skip                      -> ret ()
        | Seq (stm1, stm2)          -> stmntEval stm1 >>>= stmntEval stm2 
        | ITE (guard, stm1, stm2)   -> push >>>= boolEval guard >>= (fun x -> if x then stmntEval stm1 else stmntEval stm2 ) >>>= pop
        | While(guard, stm)         -> boolEval guard >>= (fun x -> if x then push >>>= stmntEval stm >>>= pop >>>= stmntEval (While (guard,stm)) else ret ())

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"
    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm =
        fun w pos acc -> 
        stmntEval stm >>>= lookup "_result_" |>  
        evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_acc_";"_pos_"; "_result_"])

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m =
        fun (x,y) -> 
        stmntEval stm >>>= lookup "_result_" >>= (fun whatever -> 
        ret (Map.tryFind whatever m)) |> 
        evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_";"_y_"; "_result_"])

//    type board = {
//        center        : coord
//        defaultSquare : squareFun
//        squares       : boardFun
//    }
//
//    let mkBoard c defaultSq boardStmnt ids = {
//        center = c
//        defaultSquare = defaultSq |> stmntToSquareFun 
//        squares = List.map (fun (i,s) -> (i, s |> stmntToSquareFun)) 
//            ids |> Map.ofList |> stmntToBoardFun boardStmnt
//    }
    
   
