module Eval

open StateMonad
open ScrabbleUtil
open Types

let hello = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]
let state = mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]
let emptyState = mkState [] [] []

let add a b =
    a >>= (fun a' -> b >>= (fun b' -> ret (a' + b')))

let div a b =
    a
    >>= (fun a' ->
        b
        >>= (fun b' ->
            match b' with
            | 0 -> fail DivisionByZero
            | _ -> ret (a' / b')))

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
//Mutually recursive, to make CharToInt and IntToChar work
and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N value -> ret value
    | V variable -> lookup variable
    | WL -> wordLength
    | PV aExp -> arithEval aExp >>= (fun valueOfa -> pointValue valueOfa)
    | Add (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> add (ret valueOfa1) (ret valueOfa2)))
    | Sub (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> ret (valueOfa1 - valueOfa2)))
    | Mul (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> (ret (valueOfa1 * valueOfa2))))
    | Div (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> div (ret valueOfa1) (ret valueOfa2)))
    | Mod (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 ->
            arithEval a2
            >>= (fun valueOfa2 ->
                if valueOfa2 = 0 then
                    fail DivisionByZero
                else
                    ret (valueOfa1 % valueOfa2)))
    | CharToInt cExp -> charEval cExp >>= (fun valueOfcExp -> ret (int valueOfcExp))

and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV aExp -> arithEval aExp >>= (fun valueOfaExp -> characterValue valueOfaExp)
    | ToUpper cExp -> charEval cExp >>= (fun valueOfcExp -> ret (System.Char.ToUpper valueOfcExp))
    | ToLower cExp -> charEval cExp >>= (fun valueOfcExp -> ret (System.Char.ToLower valueOfcExp))
    | IntToChar aExp -> arithEval aExp >>= (fun valueOfaExp -> ret (char valueOfaExp))

let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> ret (valueOfa1 = valueOfa2)))
    | ALt (a1, a2) ->
        arithEval a1
        >>= (fun valueOfa1 -> arithEval a2 >>= (fun valueOfa2 -> ret (valueOfa1 < valueOfa2)))
    | Not bExp -> boolEval bExp >>= (fun valueOfbExp -> ret (not valueOfbExp))
    | Conj (b1, b2) ->
        boolEval b1
        >>= (fun valueOfb1 -> boolEval b2 >>= (fun valueOfb2 -> ret (valueOfb1 && valueOfb2)))
    | IsVowel c ->
        let c = charEval c >>= (fun valueOfc -> ret (System.Char.ToLower valueOfc))

        c
        >>= (fun valueOfc ->
            ret (
                valueOfc = 'a'
                || valueOfc = 'e'
                || valueOfc = 'i'
                || valueOfc = 'o'
                || valueOfc = 'u'
            ))
    | IsLetter c -> charEval c >>= (fun valueOfc -> ret (System.Char.IsLetter valueOfc))
    | IsDigit c -> charEval c >>= (fun valueOfc -> ret (System.Char.IsDigit valueOfc))


type stmnt = (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec stmntEval stmnt : SM<unit> =
    match stmnt with
    | Declare variable -> declare variable
    | Ass (variable, aExp) -> arithEval aExp >>= (fun valueOfaExp -> update variable valueOfaExp)
    | Skip -> ret ()
    | Seq (stmnt1, stmnt2) -> stmntEval stmnt1 >>= (fun _ -> stmntEval stmnt2)
    | ITE (i, t, e) -> boolEval i >>= fun I ->
        push >>>= stmntEval (if I then t else e) >>>= pop
    | While (b, body) as wh ->
        boolEval b
        >>= (fun b ->
            if b then
                push >>>= stmntEval body >>>= pop // `>>>= stmntEval wh` missing
            else
                ret ())

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let rec arithEval2 a : SM<int> =
    match a with
    | N value -> prog { return value }
    | V variable -> prog { return! lookup variable }
    | WL -> prog { return! wordLength }
    | PV aExp ->
        prog {
            let! valueOfaExp = arithEval2 aExp
            return! pointValue valueOfaExp
        }
    | Add (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2
            return (valueOfa1 + valueOfa2)
        }
    | Sub (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2
            return (valueOfa1 - valueOfa2)
        }
    | Mul (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2
            return (valueOfa1 * valueOfa2)
        }
    | Div (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2

            if valueOfa2 = 0 then
                return! fail DivisionByZero
            else
                return (valueOfa1 / valueOfa2)
        }
    | Mod (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2

            if valueOfa2 = 0 then
                return! fail DivisionByZero
            else
                return (valueOfa1 % valueOfa2)
        }
    | CharToInt cExp ->
        prog {
            let! valueOfcExp = charEval2 cExp
            return (int valueOfcExp)
        }

and charEval2 c : SM<char> =
    match c with
    | C c -> prog { return c }
    | CV aExp ->
        prog {
            let! valueOfaExp = arithEval2 aExp
            return! characterValue valueOfaExp
        }
    | ToUpper cExp ->
        prog {
            let! valueOfcExp = charEval2 cExp
            return (System.Char.ToUpper valueOfcExp)
        }
    | ToLower cExp ->
        prog {
            let! valueOfcExp = charEval2 cExp
            return (System.Char.ToLower valueOfcExp)
        }
    | IntToChar aExp ->
        prog {
            let! valueOfaExp = arithEval2 aExp
            return (char valueOfaExp)
        }

let rec boolEval2 b : SM<bool> =
    match b with
    | TT -> prog { return true }
    | FF -> prog { return false }
    | AEq (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2
            return (valueOfa1 = valueOfa2)
        }

    | ALt (a1, a2) ->
        prog {
            let! valueOfa1 = arithEval2 a1
            let! valueOfa2 = arithEval2 a2
            return (valueOfa1 < valueOfa2)
        }
    | Not bExp ->
        prog {
            let! valueOfbExp = boolEval2 bExp
            return (not valueOfbExp)
        }
    | Conj (b1, b2) ->
        prog {
            let! valueOfb1 = boolEval2 b1
            let! valueOfb2 = boolEval2 b2
            return (valueOfb1 && valueOfb2)
        }
    | IsVowel cExp ->
        prog {
            let! valueOfc = charEval2 cExp
            let lowerCaseC = System.Char.ToLower valueOfc

            return
                (lowerCaseC = 'a'
                 || lowerCaseC = 'e'
                 || lowerCaseC = 'i'
                 || lowerCaseC = 'o'
                 || lowerCaseC = 'u')
        }
    | IsLetter c ->
        prog {
            let! valueOfc = charEval2 c
            return (System.Char.IsLetter valueOfc)
        }
    | IsDigit c ->
        prog {
            let! valueOfc = charEval2 c
            return (System.Char.IsDigit valueOfc)
        }

let rec stmntEval2 stm : SM<unit> =
    match stm with
    | Declare variable -> prog { return! declare variable }
    | Ass (variable, aExp) ->
        prog {
            let! valueOfaExp = arithEval2 aExp
            return! update variable valueOfaExp
        }
    | Skip -> prog { return! ret () }
    | Seq (stm1, stm2) ->
        prog {
            //Er det her rigtigt?
            do! stmntEval2 stm1
            do! stmntEval2 stm2
        }
    | ITE (bExp, stm1, stm2) ->
        prog {
            let! valueOfbExp = boolEval2 bExp
            do! push

            if valueOfbExp then
                do! stmntEval2 stm1
            else
                do! stmntEval2 stm2

            do! pop
        }
    | While (bExp, stm) ->
        prog {
            do! push
            let! valueOfbExp = boolEval2 bExp

            if valueOfbExp then
                do! stmntEval2 stm
                do! stmntEval2 (While(bExp, stm))
            else
                return! ret ()

            do! pop
        }

(* Part 4 (Optional) *)

let stmntToSquareFun stm  =
    //square fun is of type word, position and accumulator
    fun word pos acc ->
        let initialState =
            mkState [ ("_pos_", pos); ("_acc_", acc); ("_result_", 0) ] word [ "_pos_"; "_acc_"; "_result_" ]
        //The function should return the value of the _result_ variable of the final state
        //when the statement has been executed and fail if the evaluation fails.
        stmntEval stm >>>= lookup "_result_" |> evalSM initialState

type squareStmnt = Map<int, stmnt>

let stmntsToSquare stms : square =
    Map.map (fun priority stm -> stmntToSquareFun stm) stms

let stmntToBoardFun (stmnt: stmnt) (t: Map<int, 'a>) (coord: coord) : Result<'a option, Error> =
    let x, y = coord
    
    let initialState =
        mkState [ ("_x_", x); ("_y_", y); ("_result_", 0) ] [] [ "_x_"; "_y_"; "_result_" ]

    stmntEval stmnt >>>= lookup "_result_"
    |> evalSM initialState
    |> fun result ->
        match result with
        //after which it looks up the integer value id stored in _result_ and returns
        | Success id ->
            match Map.tryFind id t with
            | Some square -> Success(Some square)
            //Success None is not considered a failure, it just means that the coordinate is empty (outside the board, for instance).
            | None -> Success None
        //Failure if evaluation of the statement fails.
        | Failure error -> Failure error

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let mkBoard (centerCoordinate: coord) (defaultSq: int) (boardStmnt: stmnt) (ids: Map<int, squareStmnt>) : board =
    let defaultSquare = stmntsToSquare (Map.find defaultSq ids)

    let squares =
        stmntToBoardFun boardStmnt (Map.map (fun _ stmnts -> stmntsToSquare stmnts) ids)

    { center = centerCoordinate
      defaultSquare = defaultSquare
      squares = squares }
