namespace GrumboBot

open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

type dir =
    | Right
    | Down
    | Left
    | Up

// The RegEx module is only used to parse human input. It is not used for the final product.
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =
    let printHand UniqueIdsToTile handWithUniqueIds =
        handWithUniqueIds
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x UniqueIdsToTile) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type state =
        { board: Parser.board
          dict: Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          playerTurn: uint32
          // players contain both playerids and their forfeited status
          playerStatus: List<(uint32 * bool)>
          tilesAlreadyPlayed: Map<coord, uint32> }

    let mkState b d pn ptu playerAndStatus h tilesAlreadyPlayed =
        { board = b
          dict = d
          playerNumber = pn
          playerTurn = ptu
          playerStatus = playerAndStatus
          hand = h
          tilesAlreadyPlayed = tilesAlreadyPlayed }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let playerTurn st = st.playerTurn
    let tilesAlreadyPlayed st = st.tilesAlreadyPlayed

module Scrabble =
    let playGame cstream (handIdToTile:Map<uint32,Set<char * int>>) (st: State.state) =

        let rec aux (st: State.state) =

            forcePrint "HAND PRINT:\n"
            Print.printHand handIdToTile (State.hand st)

            //If count is 0. Then first move must be on center tile.
            if st.playerTurn = 1u && st.tilesAlreadyPlayed.Count = 0 then
                forcePrint "Ready to do first move\n"
                //Do FindMoves with Cood (0,0)
            else
                forcePrint "I am not first player\n"

            let convertHandToListOfChar (handIdToTile: Map<uint32, 'a>) hand=
                MultiSet.fold (
                    fun acc id amountOfTilesOfThisId ->
                        let (pieces: Set<char*int>) = (Map.find id handIdToTile)
                        Set.fold (fun setacc (char, pV) ->
                            //Add char times amountOfTilesOfThisId to acc
                            let rec recur acc char amountOfTilesOfThisId=
                                if amountOfTilesOfThisId = 0 then acc
                                else recur (char :: acc) char (amountOfTilesOfThisId - 1)
                            recur acc char (int amountOfTilesOfThisId)
                        ) [] pieces
                ) [] hand
            
            //Hand as chars    
            let handAsListOfChar = convertHandToListOfChar handIdToTile st.hand
            //Hand as string
            let ListOfCharAsString = String.concat "" (List.map (fun x -> string x) handAsListOfChar)
            
            let createEveryStringPermutationOfWord (word: string) =
                let rec loop word =
                    match word with
                    | "" -> []
                    | _ ->
                        let rec recur acc word =
                            match word with
                            | "" -> acc
                            | _ ->
                                let newWord = word.Substring(1)
                                recur (word :: acc) newWord
                        recur [] word
                loop word

            let rec permutations (str:string) =
                match str with
                | "" -> [""]
                | _ ->
                    [ for i = 0 to str.Length - 1 do
                          for p in permutations (str.Remove(i, 1)) do
                              yield str.[i..i] + p ]

            let everyStringPermutationOfHand = createEveryStringPermutationOfWord ListOfCharAsString
            let permutationTest = permutations ListOfCharAsString
            forcePrint (sprintf "Every string permutation of hand: %A\n" everyStringPermutationOfHand)
            forcePrint (sprintf "Permutation test: %A\n" permutationTest)
            
            let lookupWithDict char (dict:Dict) =
                let rec loop char (dict:Dict)=
                    let newDict = Dictionary.step char dict
                    match newDict with
                    | Some (foundWord, newDict) ->
                        forcePrint (sprintf "foundWord: %A\n" foundWord)
                        forcePrint (sprintf "NewDict: %A\n" newDict)
                        loop char newDict
                    | _ ->
                        forcePrint "Empty"
                        false
                loop char dict

            let resultOfLookup = lookupWithDict 'A' st.dict
            forcePrint (sprintf "Result of lookup: %A\n" resultOfLookup)
            
            let lookUpFullWordDict word (dict:Dict) =
                let lookupResult = Dictionary.lookup word dict
                match lookupResult with
                | true ->
                    forcePrint (sprintf "Found word: %A\n" word)
                    true
                | false ->
                     forcePrint (sprintf "DID NOT FIND WORD: %A\n" word)
                     false
            
            
            //let findMoves (st: State.state) (iniDict: Dict) (handIdToTile: Map<uint32, 'a>) (iniCoord : coord) (dir: dir)=
            //    let rec recur (dict: Dict) (hand: MultiSet.MultiSet<uint32>) (wordInPlay: (coord * (uint32 * (char * int))) list) (coord: coord) : (coord * (uint32 * (char * int))) list=
            //        MultiSet.fold (
            //            fun acc id amountOfTilesOfThisId ->
            //                let (pieces: Set<char*int>) = (Map.find id handIdToTile)
            //                forcePrint (sprintf "acc: %A\n" acc)
            //                forcePrint (sprintf "Pieces: %A\n" pieces)
            //                Set.fold (fun setacc (char, pV) ->
            //                    forcePrint (sprintf "Char: %A\n" char)
            //                    forcePrint (sprintf "PV: %A\n" pV)
            //                    forcePrint (sprintf "setacc: %A\n" setacc)  
            //                    //Add to setacc
            //                    let newWordInPlay = (coord, (id, (char, pV))) :: wordInPlay
            //                    forcePrint (sprintf "wordInPlay: %A\n" wordInPlay)
            //                    wordInPlay
            //                ) [] pieces
            //        ) [] hand
            //    recur iniDict st.hand [] iniCoord
//
            //let moveFinder = findMoves st st.dict handIdToTile (0,0) Right
            //forcePrint (sprintf "MoveFinder: %A\n" moveFinder)
            
            let input = System.Console.ReadLine()
            let move = RegEx.parseMove input

            //If move is empty, then change pieces logic
            
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                forcePrint "CMPlaySuccess prints: \n"
                //Move made: 0 0 1A1 0 1 20T1
                //forcePrint (sprintf "MS: %A\n" ms) //[((0, 0), (1u, ('A', 1))); ((0, 1), (20u, ('T', 1)))] <- Tiles played, remove from hand.
                //forcePrint (sprintf "Points: %A\n" points) //2
                //forcePrint (sprintf "NewPieces: %A\n" newPieces) //NewPieces: [(6u, 1u); (14u, 1u)]  <- New tiles gotten in hand

                //SAVE MOVES MADE TO TILESALREADYPLAYED
                let rec addMovesToTilesAlreadyPlayed ms =
                    match ms with
                    | [] -> st.tilesAlreadyPlayed
                    | h :: t -> addMovesToTilesAlreadyPlayed t |> Map.add (fst h) (snd h |> fst)

                let tilesAlreadyPlayed = addMovesToTilesAlreadyPlayed ms
                forcePrint (sprintf "Tiles ALREADY played: %A\n" tilesAlreadyPlayed.Count)

                //REMOVE AND ADD NEW TILES
                let rec removeUsedTiles (ms: uint32 list) (hand: MultiSet.MultiSet<uint32>) =
                    match ms with
                    | [] -> hand
                    | h :: t -> removeUsedTiles t (MultiSet.removeSingle h hand)

                let rec addNewGainedTiles (newPieces: (uint32 * uint32) list) (hand: MultiSet.MultiSet<uint32>) =
                    match newPieces with
                    | [] -> hand
                    | h :: t -> addNewGainedTiles t (MultiSet.addSingle (fst h) hand)

                let handAfterRemoval =
                    removeUsedTiles (List.map (fun x -> snd x) ms |> List.map (fun x -> fst x)) (State.hand st)

                let handAfterAddingAndRemoving = addNewGainedTiles newPieces handAfterRemoval

                let st' =
                    { st with
                        hand = handAfterAddingAndRemoving
                        tilesAlreadyPlayed = tilesAlreadyPlayed }

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                forcePrint "CMPlayed prints: \n"
                forcePrint (sprintf "pid: %A\n" pid)
                forcePrint (sprintf "MS: %A\n" ms)
                forcePrint (sprintf "Points: %A\n" points)

                //Add moves ms to tilesAlreadyPlayed
                let rec addMovesToTilesAlreadyPlayed ms =
                    match ms with
                    | [] -> st.tilesAlreadyPlayed
                    | h :: t -> addMovesToTilesAlreadyPlayed t |> Map.add (fst h) (snd h |> fst)

                let tilesAlreadyPlayed = addMovesToTilesAlreadyPlayed ms
                forcePrint (sprintf "Tiles ALREADY played: %A\n" tilesAlreadyPlayed.Count)
                let st' = { st with tilesAlreadyPlayed = tilesAlreadyPlayed }
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                forcePrint "CMPlayFailed prints: \n"
                forcePrint (sprintf "pid: %A\n" pid)
                forcePrint (sprintf "MS: %A\n" ms)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMForfeit player) ->
                forcePrint "CMForfeit prints: \n"

                let rec changePlayerStatus player =
                    match st.playerStatus with
                    | [] -> []
                    | h :: t ->
                        if fst h = player then
                            (fst h, true) :: t
                        else
                            h :: changePlayerStatus player

                //Print player status
                let rec printPlayerStatus playerStatus =
                    match playerStatus with
                    | [] -> ()
                    | h :: t ->
                        forcePrint (sprintf "Player %d: %A\n" (fst h) (snd h))
                        printPlayerStatus t

                printPlayerStatus st.playerStatus

                let st' = { st with playerStatus = changePlayerStatus player }
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st

        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        // Converter to make Map<unit32,tile> into Map<uint32,(char*int)>
        let getFirstElement tile = Set.toList tile |> (fun x -> x.Head)
        let gameStartTiles = Map.map (fun id tileTuple -> getFirstElement tileTuple) tiles

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber playerTurn List.empty handSet Map.empty)
