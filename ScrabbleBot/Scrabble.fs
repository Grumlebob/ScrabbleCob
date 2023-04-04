namespace CarstenBot
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
 type dir =
        | Right | Down | Left | Up
        
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        playerTurn    : uint32
        // players contain both playerids and their forfeited status
        players       : List<(uint32 * bool)>
        hand          : MultiSet.MultiSet<uint32>
        playedTiles   : Map<coord, char * Map<dir, (uint32 * char) list * uint32>>
        remainTiles   : uint32
        }

    let mkState b d pn ptu p h pti rT = {board = b; dict = d; playerNumber = pn; playerTurn = ptu; players = p; hand = h; playedTiles = pti; remainTiles = rT}

    let board st                = st.board
    let dict st                 = st.dict
    let playerNumber st         = st.playerNumber
    let hand st                 = st.hand
    let playerTurn st           = st.playerTurn
    let playedTiles st          = st.playedTiles

module Scrabble =
    let changePlayer (pn:uint32) =
       match pn with
       | 1u -> 2u
       | 2u -> 1u
       | _ -> 0u
    
    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let checkForWord (st: State.state) (iniDict: Dictionary.Dict) (dir: dir) (coord: coord) =
                match Map.tryFind coord st.playedTiles with
                | Some (char, value) ->
                    match Map.tryFind dir value with
                    | Some (word, pos) ->
                        List.fold (
                            fun acc (id, char) ->
                                match Dictionary.step char acc with
                                | Some (_, dict) -> dict
                                | None -> iniDict
                            ) iniDict word
                    | None ->
                        match Dictionary.step char iniDict with
                        | Some (_, dict) -> dict
                | None -> iniDict
                
            let checkCharAdjacentValidity (st: State.state) (dir: dir) (x,y) (char: char)=
                let rec backCheck dir (x,y) =
                    let (x', y') =
                        match dir with
                        | Up | Down -> (x-1, y)
                        | Right | Left -> (x, y-1)
                    match Map.tryFind (x', y') st.playedTiles with
                    | Some (c, map) -> false
                    | None -> true
                let rec frontCheck dir (x,y) =
                    let (x', y') =
                        match dir with
                        | Up | Down -> (x+1, y)
                        | Right | Left -> (x, y+1)
                    match Map.tryFind (x', y') st.playedTiles with
                    | Some (c, map) -> false
                    | None -> true
                frontCheck dir (x,y) && backCheck dir (x,y)
            
            let checkAdjacentWord (st: State.state) (dir: dir) (word: (coord * (uint32 * (char * int))) list) =
                let firstCoord = fst word[0]
                let nextCoord =
                    match dir with
                    | Right -> (fst firstCoord-1, snd firstCoord)
                    | Down -> (fst firstCoord, snd firstCoord-1 )
                let dict =
                    match Map.tryFind nextCoord st.playedTiles with
                    | Some (char, map) ->
                            match Map.tryFind dir map with
                            | Some (word, pos) ->
                                List.fold (fun acc (id, char) ->
                                    match Dictionary.step char acc with
                                    | Some (bool, dict) -> dict
                                    | None -> acc
                                ) st.dict word
                            | None ->
                                match Dictionary.step char st.dict with
                                    | Some (bool, dict) -> dict
                                    | None -> st.dict
                    | None -> st.dict
                let dict' = List.fold (
                                 fun acc ((coord: coord), (id,(char, pV))) ->
                                    match snd acc with
                                    | Some dict ->
                                        match Dictionary.step char dict with
                                        | Some (bool, dict) -> (bool, Some dict)
                                        | None -> (false, None)
                                    | None -> (false, None)
                                 )
                                (true, Some dict) word
                let dict'' =
                    let lastCoord = fst word[word.Length-1]
                    let nextCoord =
                        match dir with
                        | Right -> (fst lastCoord+1, snd lastCoord)
                        | Down -> (fst lastCoord, snd lastCoord+1 )
                    match Map.tryFind nextCoord st.playedTiles with
                    | Some (char, map) ->
                            match Map.tryFind dir map with
                            | Some (word, pos) ->
                                List.fold (fun acc (id, char) ->
                                    match snd acc with
                                    | Some dict ->
                                        match Dictionary.step char dict with
                                        | Some (bool, dict) -> (bool, Some dict)
                                        | None -> (false, None)
                                    | None -> (false, None)
                                ) dict' word
                            | None ->
                                match snd dict' with
                                | Some dict -> 
                                    match Dictionary.step char dict with
                                        | Some (bool, dict) -> (bool, Some dict)
                                        | None -> (false, None)
                                | None -> (false, None)
                    | None -> dict'
                fst dict''
                    
                
            let findMoves (st: State.state) (iniDict: Dictionary.Dict) (allPieces: Map<uint32, 'a>) (iniCoord : coord) (dir: dir)=
                let rec recur (dict: Dictionary.Dict) (hand: MultiSet.MultiSet<uint32>) (wordInPlay: (coord * (uint32 * (char * int))) list) (coord: coord)=
                    MultiSet.fold (
                        fun acc id amount ->
                            let (pieces: Set<char*int>) = (Map.find id allPieces)
                            Set.fold (fun setacc (char, pV) ->
                                let nextPos : coord =
                                    match dir with
                                    | Right -> (fst coord + 1, snd coord)
                                    | Down -> (fst coord, snd coord + 1)
                                match Map.tryFind nextPos st.playedTiles with
                                | None ->
                                    let newDict = Dictionary.step char dict
                                    let newUsablePieces = MultiSet.removeSingle id hand
                                    match newDict with
                                    | Some (isWord, dict) ->
                                        let newWord = wordInPlay@[(fst nextPos, snd nextPos), (id, (char, pV))]
                                        let canPlace =
                                                checkCharAdjacentValidity st dir nextPos char &&
                                                if isWord then
                                                    checkAdjacentWord st dir newWord
                                                else true
                                        if canPlace then
                                            if isWord then
                                                newWord::acc@(recur dict newUsablePieces newWord nextPos)
                                            else
                                                acc@(recur dict newUsablePieces newWord nextPos)
                                        else acc
                                    | None -> acc
                                | Some _ -> acc
                            ) [] pieces
                    ) [] hand
                recur iniDict st.hand [] iniCoord
                
            let findAllMoves (st: State.state) (dict: Dictionary.Dict) (allPieces: Map<uint32, 'a>) =
                Map.fold (
                    fun acc coord (char, map) ->
                            let rightDict = checkForWord st dict Right coord
                            let downDict = checkForWord st dict Down coord
                            let rightMoves = findMoves st rightDict allPieces coord Right
                            let downMoves = findMoves st downDict allPieces coord Down
                            downMoves @ rightMoves @ acc
                    ) [] st.playedTiles
            let enterMode = false
                
            if(enterMode) then
                debugPrint (sprintf "Press enter to play\n")
                let input = System.Console.ReadLine()
                ()   
            //let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input
            let moves =
                if st.playedTiles.Count = 0 then
                    let moves = findMoves st st.dict pieces (-1, 0) Right
                    List.sortByDescending (fun m -> List.length m) moves
                else 
                    let moves = findAllMoves st st.dict pieces
                    List.sortByDescending (fun m -> List.length m) moves
            let mutable exchangePieces = []
            //printfn "Total Move Count %d" moves.Length
            if moves.Length = 0 then
                if st.remainTiles < MultiSet.size st.hand then
                    exchangePieces <- (MultiSet.toList st.hand)[0 .. int(st.remainTiles)]
                else
                    exchangePieces <- MultiSet.toList st.hand
                send cstream (SMChange exchangePieces)
            else
                let move = moves[0]
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)
            
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlayed (pid, move, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st
                aux st'
            | RCM (CMPlaySuccess(move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //playedTiles   : Map<coord, char * Map<dir, (uint32 * char) list * uint32>>
                let st' =
                    //Check if only one letter was placed
                    let firstCoord = fst move[0]
                    let moveDir =
                        if move.Length = 1 then
                            Right
                        else 
                            let secondCoord = fst move[1]
                            if (fst firstCoord) = (fst secondCoord) then
                               Down
                            else Right
                    let mutable i = 0u
                    let boardMove =
                        match moveDir with
                        | Right ->
                            match Map.tryFind ((fst firstCoord)-1, (snd firstCoord)) st.playedTiles with
                            | Some (char, map) ->
                                match Map.tryFind Right map with
                                | Some (word, pos) -> word
                                | None ->
                                    match Map.tryFindKey (
                                        fun key value -> not (key = 0u) && Set.exists (fun (c, pV) -> c = char) value) pieces with
                                    | Some id -> [(id, char)]
                                    | None -> []
                            | None -> []
                        | Down ->
                            match Map.tryFind ((fst firstCoord), (snd firstCoord)-1) st.playedTiles with
                            | Some (char, map) ->
                                match Map.tryFind Right map with
                                | Some (word, pos) -> word
                                | None ->
                                    match Map.tryFindKey (
                                        fun key value -> not (key = 0u) && Set.exists (fun (c, pV) -> c = char) value) pieces with
                                    | Some id -> [(id, char)]
                                    | None -> []
                            | None -> []
                    List.fold (
                        fun (acc: State.state) (coord, (id ,(char, value))) ->
                            let hand = MultiSet.removeSingle id acc.hand
                            let wordAtCoord =
                                match Map.tryFind coord acc.playedTiles with
                                | Some (char, map) ->
                                    let move1 = (boardMove @ (List.map (fun (coord, (id, (char, pv))) -> (id, char)) move), i)
                                    Map.add moveDir move1 map
                                | None ->
                                    let move2 = (boardMove @ (List.map (fun (coord, (id, (char, pv))) -> (id, char)) move), i)
                                    Map.add moveDir move2 Map.empty
                            let playedTiles' = Map.add coord (char, wordAtCoord) acc.playedTiles
                            i <- i+1u
                            State.mkState acc.board acc.dict acc.playerNumber acc.playerTurn acc.players hand playedTiles' acc.remainTiles
                        ) st move
                let st'' =
                    List.fold (
                        fun (acc: State.state) (id, amount) ->
                            let newhand = MultiSet.add id amount acc.hand
                            let remaingTiles = acc.remainTiles - amount
                            State.mkState acc.board acc.dict acc.playerNumber acc.playerTurn acc.players newhand acc.playedTiles remaingTiles
                    ) st' newPieces
                debugPrint (sprintf "###\n updated state %A \n### \n" st'') 
                aux st''
            | RCM (CMPlayFailed (pid, move)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState st.board st.dict st.playerNumber st.playerTurn st.players st.hand st.playedTiles st.remainTiles
                debugPrint (sprintf "############ PLAY FAILED ###########")
                aux st'
            | RCM (CMPassed (pid)) ->
                let st' = st // Update this
                debugPrint (sprintf "############ PLAYER PASSED ###########")
                aux st'
            | RCM (CMForfeit (pid)) ->
                debugPrint (sprintf "############ PLAYER FORFEITED #########")
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMChange (pid, numberOfTiles)) ->
                debugPrint (sprintf "############ OTHER PLAYER CHANGED PIECES ###########")
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess (newTiles)) ->
                debugPrint (sprintf "############ PLAYER CHANGED PIECES ###########")
                let hand' =
                    List.fold ( fun acc (id, amount) -> MultiSet.add id amount acc ) (List.fold ( fun acc id -> MultiSet.removeSingle id acc ) st.hand exchangePieces) newTiles
                let st' = State.mkState st.board st.dict st.playerNumber st.playerTurn st.players hand' st.playedTiles st.remainTiles
                aux st'
            | RCM (CMTimeout (pid)) ->
                debugPrint (sprintf "############ PLAYER TIMED OUT ###########")
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            //| RCM a -> failwith (sprintf "not implemented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        // Converter to make Map<unit32,tile> into Map<uint32,(char*int)>
        let getFirstElement tile = Set.toList tile |> (fun x -> x.Head)
        let gameStartTiles = Map.map (fun id tileTuple -> getFirstElement tileTuple) tiles

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber playerTurn List.empty handSet Map.empty 100u)
        