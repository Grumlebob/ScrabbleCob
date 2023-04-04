module StateMonad

//Think of this as TwoWay tracks. One for the input and one for the output.
type Error =
    | VarExists of string (* Declaring an existing variable in the same scope *)
    | VarNotFound of string (* Referencing a variable that does not exist *)
    | IndexOutOfBounds of int (* Trying to get the point or character value that is outside the index of the word *)
    | DivisionByZero (* Division or modulo by zero *)
    | ReservedName of string (* Trying to declare a reserved name *)

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b

type State =
    { vars: Map<string, int> list //Stack of maps. Variable to value.
      word: (char * int) list //List of characters and their position in the word.
      reserved: Set<string> } //Set of reserved words.

//The intuition of vars is that it is a stack of variable bindings where an empty map (an empty variable
//environment) is pushed on the stack when entering a block (in a while-loop for instance) and popped when
//you exit the block, effectively forgetting any variable that was declared in the block.

//variables declared within a loop or branch of an if-statement are: not available outside these sections
// shadow (replace) any already existing variable with the same name while inside these sections
type SM<'a> = S of (State -> Result<'a * State, Error>)

let mkState lst word reserved =
    { vars = [ Map.ofList lst ]
      word = word
      reserved = Set.ofList reserved }
//This is our last call in the twoWay track
let evalSM (s: State) (S a: SM<'a>) : Result<'a, Error> =
    match a s with
    | Success (result, _) -> Success result
    | Failure error -> Failure error
//Convert to twoWay track
let bind (f: 'a -> SM<'b>) (S a: SM<'a>) : SM<'b> =
    S(fun s ->
        match a s with
        | Success (b, s') ->
            match f b with
            | S g -> g s'
        | Failure err -> Failure err)

//Return
let ret (v: 'a) : SM<'a> = S(fun s -> Success(v, s))
//"None"
let fail err : SM<'a> = S(fun s -> Failure err)
//since push has the type SM<unit> we use >>>= to compose it with the rest of the program and not >>= .
//see how we use >>= , rather than >>>= to use the result for lookup in later parts of a program.
let (>>=) x f = bind f x
let (>>>=) x f = x >>= (fun () -> f)

let push: SM<unit> = S(fun s -> Success((), { s with vars = Map.empty :: s.vars })) // map :: s.vars is adding to FRONT of list.
//Push an empty map on the stack. (I.e. entering a while loop, we get a fresh slate).
// () is the return value, and why it is SM<unit> and not SM<map<string, int>>.
//The notation {r with fieldName = x} is used to creates a copy of the record r except for the field
//fieldName which is set to x in stead of its original value. The rest of the record, no matter its size, is left unchanged.

let pop: SM<unit> =
    S(fun state -> Success((), { state with vars = state.vars |> List.tail }))

let wordLength: SM<int> = S(fun state -> Success(state.word.Length, state))

//The character at position x in the word.
let characterValue (pos: int) : SM<char> =
    S(fun state ->
        if pos < 0 || pos >= state.word.Length then
            Failure(IndexOutOfBounds pos)
        else
            Success(state.word.[pos] |> fst, state))

let pointValue (pos: int) : SM<int> =
    //Same as charvalue, instead of fst for the char, we do snd for the value.
    S(fun state ->
        if pos < 0 || pos >= state.word.Length then
            Failure(IndexOutOfBounds pos)
        else
            Success(state.word.[pos] |> snd, state))

let lookup (variableToFind: string) : SM<int> =
    let rec loop =
        function
        | [] -> None
        | headMap :: tailMaps ->
            match Map.tryFind variableToFind headMap with
            | Some value -> Some value
            | None -> loop tailMaps

    //Initial state is unchanged. Lookups do not change the state.
    S(fun state ->
        match loop (state.vars) with
        | Some v -> Success(v, state)
        | None -> Failure(VarNotFound variableToFind))


let declare (varToDeclare: string) : SM<unit> =
    S(fun state ->
        match state.vars with
        //3. If the stack is empty then the function is undefined and you can do anything you like, including failing
        | [] -> failwith "stack is empty, crashing - should never happen."
        | headMap :: tailMaps ->
            //1. If the variable has the same name as one of the reserved words, fail with ReservedName x
            if Set.contains varToDeclare state.reserved then
                Failure(ReservedName varToDeclare)
            //2. If the variable already exists in the top variable environment, fail with VarExists x
            else if headMap.ContainsKey varToDeclare then
                Failure(VarExists varToDeclare)
            //If nothing fails:
            //Adds that variable to the top variable environment of the stack with a starting value of 0
            else
                Success((), { state with vars = Map.add varToDeclare 0 headMap :: tailMaps }))

//Simply finds the first instance of the variable.
//Copies the entire state, except for the vars field, which is updated with the new value.
let update (variableToUpdate: string) (newValue: int) : SM<unit> =
    let rec loop indexOfFoundVariable =
        function
        | [] -> None
        | headMap :: tailMaps ->
            match Map.tryFind variableToUpdate headMap with
            //We found the first instance of the variable in the stack. Return the index of the map in the stack, with the variable.
            | Some _ -> Some indexOfFoundVariable
            //Keep going through the stack, to look for the variable.
            | None -> loop (indexOfFoundVariable + 1) tailMaps

    S(fun state ->
        //Find the variable with the helper method.
        match loop 0 (state.vars) with
        //Variable was found at indexOfVariableInStackOfMaps in the stack state.vars
        | Some indexOfVariableInStackOfMaps ->
            Success(
                (),
                { state with
                    vars =
                        //Go through the stack. Only the change the map at the index of the variable, to the new updated value.
                        (state.vars
                         |> List.mapi (fun mapStackIndex (map: Map<string, int>) ->  //MapI so we can index, for every map in the stack
                             match mapStackIndex with
                             //Only change the map at the index we found.
                             | mapStackIndex when mapStackIndex = indexOfVariableInStackOfMaps ->
                                 Map.add variableToUpdate newValue map
                             //Rest of the state is unchanged.
                             | _ -> map)) }
            )
        //Variable was not found in the entire stack.
        | None -> Failure(VarNotFound variableToUpdate))
