namespace GrumboBot

open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

type Direction =
    | Right
    | Down
    | Up
    | Left

type WhereToAddFixedChar =
    | Front
    | Back
    | DontAdd
    
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

            //Wildcard check
            //if MultiSet.contains 0u (State.hand st) then
            //    forcePrint "I have a wildcard\n"
            //else
            //    forcePrint "I MISS wildcard\n"
            //    exit 0
     
            //If count is not 0. Then first move can be anywhere.
            
            let getNewCoord (coord:coord) (direction:Direction) : coord =
                match direction with
                | Right -> (fst coord+1, snd coord)
                | Down -> (fst coord, snd coord+1)
                | Left -> (fst coord-1, snd coord)
                | Up -> (fst coord, snd coord-1)
                
            let convertHandToListWithFullInfo handIdToTile hand : (char * uint32 * int) list =
                MultiSet.fold (
                    fun acc id amountOfTilesOfThisId ->
                        //TODO FIX WILD CARD. RIGHT NOW I DO SKIP WILDCARD. GENERATES TOO MANY PERMUTATIONS
                        if id = 0u then acc
                        else
                            //Get all pieces of this id
                            let (pieces: Set<char*int>) = (Map.find id handIdToTile)
                            Set.fold (fun setacc (char, pV) ->
                                //Add (char, id, pV) times amountOfTilesOfThisId to acc
                                let rec recur acc char amountOfTilesOfThisId=
                                    if amountOfTilesOfThisId = 0 then acc
                                    else recur ((char, id, pV) :: acc) char (amountOfTilesOfThisId - 1)
                                recur setacc char (int amountOfTilesOfThisId)
                            ) [] pieces
                            |> List.append acc
                ) [] hand
                                           
            //Get every combination and permutation of some tiles
            let convertListOfCharsToEveryPermutation (charList: (char * uint32 * int) list) : string list=
                //https://stackoverflow.com/questions/1526046/f-permutations
                let rec distribute e = function
                  | [] -> [[e]]
                  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
                let rec permute = function
                  | [] -> [[]]
                  | e::xs -> List.collect (distribute e) (permute xs)
                
                //https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f  
                let rec comb n l = 
                    match n, l with
                    | 0, _ -> [[]]
                    | _, [] -> []
                    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

                let convertListOfCharsToString (listOfChars:(char * uint32 * int) list) =
                    let rec loop acc listOfChars =
                        match listOfChars with
                        | [] -> acc
                        | (c, _, _) :: xs -> loop (acc + string c) xs
                    loop "" listOfChars
                
                //find combinations from length 2 till handsize and save all results to list
                let allCombinations = List.map (fun x -> comb x charList) (List.init (List.length charList) (fun x -> x + 2)) |> List.concat
                //Call permute on all combinations
                let allPermutations = List.map (fun x -> permute x) allCombinations |> List.concat
                //Coonvert all permutations to strings
                let allStrings = List.map (fun x -> convertListOfCharsToString x) allPermutations
                allStrings
            
            let convertListOfCharsToEveryPermutationWithFixedCharPlaced (charToPlace: char) (whereToPlace: WhereToAddFixedChar) (charList: (char * uint32 * int) list) : string list=
                //Place charToPlace at the beginning of every string
                let addCharToListFront (charToAdd: char) (listOfString: string list) =
                    listOfString |> List.map (fun str -> (string charToAdd) + str)

                let addCharToListBack (charToAdd: char) (listOfString: string list) =
                    listOfString |> List.map (fun str -> str + (string charToAdd))
                    
                //https://stackoverflow.com/questions/1526046/f-permutations 
                let rec distribute e = function
                  | [] -> [[e]]
                  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
                
                //Add charToPlace to the beginning of every list 
                let rec permute = function
                  | [] -> [[]]
                  | e::xs -> List.collect (distribute e) (permute xs)
                
                //https://stackoverflow.com/questions/1222185/most-elegant-combinations-of-elements-in-f  
                let rec comb n l = 
                    match n, l with
                    | 0, _ -> [[]]
                    | _, [] -> []
                    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

                let convertListOfCharsToString (listOfChars:(char * uint32 * int) list) =
                    let rec loop acc listOfChars =
                        match listOfChars with
                        | [] -> acc
                        | (c, _, _) :: xs -> loop (acc + string c) xs
                    loop "" listOfChars
                
                //find combinations from length 1 till handsize and save all results to list
                let allCombinations = List.map (fun x -> comb x charList) (List.init (List.length charList) (fun x -> x + 1)) |> List.concat
                //Call permute on all combinations
                let allPermutations = List.map (fun x -> permute x) allCombinations |> List.concat
                //Coonvert all permutations to strings
                let allStrings = List.map (fun x -> convertListOfCharsToString x) allPermutations
                //Place charToPlace at the beginning of every string
                let finalStrings = 
                    match whereToPlace with
                    | Front -> addCharToListFront charToPlace allStrings
                    | Back -> addCharToListBack charToPlace allStrings
                    | DontAdd -> allStrings
                finalStrings
            
            let lookupWordsInDict (words: string list) (dict:Dict) =
                let rec lookupEveryStringInPermutationTest acc (permutationTest: string list) (dict:Dict) =
                    match permutationTest with
                    | [] -> []
                    | h :: t ->
                        let result = Dictionary.lookup h dict
                        if result = true then
                            h :: acc @ lookupEveryStringInPermutationTest acc t dict
                        else
                            lookupEveryStringInPermutationTest acc t dict
                lookupEveryStringInPermutationTest [] words dict
            
            
            //Get the longest word from resultOfLookupEveryStringInPermutationTest
            //Change to get highest points? - Change to validation before?
            let getLongestWordFromResult (resultOfLookupEveryStringInPermutationTest:string list) =
                let rec getLongestWordFromResult (acc:string) (resultOfLookupEveryStringInPermutationTest:string list) =
                    match resultOfLookupEveryStringInPermutationTest with
                    | [] -> acc
                    | h :: t ->
                        if h.Length > acc.Length then
                            getLongestWordFromResult h t
                        else
                            getLongestWordFromResult acc t
                getLongestWordFromResult "" resultOfLookupEveryStringInPermutationTest
            
            let checkIfPlacementIsPossible (lenghtToCheck:int) (coord:coord) (direction: Direction) :(bool*Direction)=
                forcePrint (sprintf "Checking Coord: %A\n" coord)       
                let rec checkIfTileIsAlreadyPlayed (playAvailable:bool)(coord:coord) lenghtToCheck =
                    if lenghtToCheck = 0 then
                        (playAvailable, direction)
                    else
                        let horizontal = match direction with
                                         | Right -> true
                                         | Left -> true
                                         | _ -> false
                        let bothSidesFree = 
                            match horizontal with
                            | false ->
                                let sideAFree = Map.tryFind (fst coord, snd coord+1) st.tilesAlreadyPlayed
                                let sideBFree = Map.tryFind (fst coord, snd coord-1) st.tilesAlreadyPlayed
                                if sideAFree = None && sideBFree = None then
                                    true
                                else
                                    //forcePrint (sprintf "Horizontal: %A - Coord: %A - sideAFree: %A\n" horizontal coord sideAFree)
                                    //forcePrint (sprintf "Horizontal: %A - Coord: %A - sideBFree: %A\n" horizontal coord sideBFree)
                                    false
                            | true ->
                                let sideAFree = Map.tryFind (fst coord+1, snd coord) st.tilesAlreadyPlayed
                                let sideBFree = Map.tryFind (fst coord-1, snd coord) st.tilesAlreadyPlayed
                                if sideAFree = None && sideBFree = None then
                                    true
                                else
                                    //forcePrint (sprintf "Horizontal: %A - Coord: %A - sideAFree: %A\n" horizontal coord sideAFree)
                                    //forcePrint (sprintf "Horizontal: %A - Coord: %A - sideBFree: %A\n" horizontal coord sideBFree)
                                    false
                        if bothSidesFree then    
                            match Map.tryFind coord st.tilesAlreadyPlayed with
                            | Some _ ->
                                playAvailable = false |> ignore
                                (playAvailable, direction)
                            | None ->
                                let newCoord = getNewCoord coord direction
                                checkIfTileIsAlreadyPlayed playAvailable newCoord (lenghtToCheck - 1)
                        else
                            forcePrint (sprintf "Both sides are not free\n")
                            (false, direction)
                checkIfTileIsAlreadyPlayed true coord lenghtToCheck
            
            let playLongestWord (longestWord:string) (startCoord: coord) (handWithFullInfo:(char * uint32 * int) list) (direction: Direction) : (coord * (uint32 * (char * int))) list =
                let charList = List.ofArray (longestWord.ToCharArray())
                let rec calculateMove (acc:(coord * (uint32 * (char * int))) list) (chars:char list) (currentCoord:coord) =
                    match chars with
                    | [] -> acc
                    | h :: t ->
                        let (char, id, pV) = List.find (fun (c, _, _) -> c = h) handWithFullInfo
                        let newMove = (currentCoord), (id, (char, pV))
                        let newCoord = getNewCoord currentCoord direction
                        
                        calculateMove (newMove :: acc) t newCoord
                calculateMove [] charList startCoord
            
            
            let checkAllDirectionsAndPlay (longestWord:string) (startCoord: coord) (handWithFullInfo:(char * uint32 * int) list) =
                let checkIfPossibleToTheRight = checkIfPlacementIsPossible longestWord.Length startCoord Right
                let checkIfPossibleToTheLeft = checkIfPlacementIsPossible longestWord.Length startCoord Down
                let checkIfPossibleToTheUp = checkIfPlacementIsPossible longestWord.Length startCoord Up
                let checkIfPossibleToTheDown = checkIfPlacementIsPossible longestWord.Length startCoord Down
                let allDirections = [checkIfPossibleToTheRight; checkIfPossibleToTheLeft; checkIfPossibleToTheUp; checkIfPossibleToTheDown]
                
                let rec checkAllDirectionsAndPlay (allDirections:(bool*Direction) list) =
                    match allDirections with
                    | [] ->
                        forcePrint ("No possible move: Please write one yourself:\n")
                        let input = System.Console.ReadLine()
                        let move = RegEx.parseMove input
                        send cstream (SMPlay move)
                    | h :: t ->
                        match h with
                        | (true, direction) ->
                            let move = playLongestWord longestWord (0,0) handWithFullInfo direction
                            send cstream (SMPlay move)
                        | _ ->
                            checkAllDirectionsAndPlay t
                checkAllDirectionsAndPlay allDirections

            //Find some coordinate, where there is room to play.
            let generateListOfPossibleStartCoords =
                let listOfCoordsFromTilesPlayed = Map.toList st.tilesAlreadyPlayed |> List.map (fun (coord, _) -> coord)
                
                let rec generateListOfPossibleStartCoords (lenghtToCheck: int) (coords: coord list) (acc: (coord * Direction) list)  =
                    match coords with
                    | [] -> acc
                    | coord :: t ->
                        let checkCoordRight = checkIfPlacementIsPossible lenghtToCheck coord Right
                        let checkCoordLeft = checkIfPlacementIsPossible lenghtToCheck coord Left
                        let checkCoordUp = checkIfPlacementIsPossible lenghtToCheck coord Up
                        let checkCoordDown = checkIfPlacementIsPossible lenghtToCheck coord Down
                        let allDirections = [checkCoordRight; checkCoordLeft; checkCoordUp; checkCoordDown]
                        let rec checkAllDirections (allDirections:(bool*Direction) list) (acc: (coord * Direction) list)  =
                            match allDirections with
                            | [] ->
                                forcePrint (sprintf "listOfPossibleStartCoords: %A\n" acc)
                                acc
                            | h :: t ->
                                match h with
                                | (true, direction) ->
                                    checkAllDirections t ((coord, direction) :: acc)
                                | _ ->
                                    checkAllDirections t acc
                        let directionsList = checkAllDirections allDirections acc
                        generateListOfPossibleStartCoords lenghtToCheck t directionsList
                //TODO HARDCODED LENGHT 7        
                generateListOfPossibleStartCoords 7 listOfCoordsFromTilesPlayed []
            
            
            //Hand as chars*id*pointvalue    
            let handWithFullInfo = convertHandToListWithFullInfo handIdToTile st.hand
            //forcePrint (sprintf "handAsListOfChar: %A\n" handWithFullInfo)
            
            //Every permutation of hand
            //let everyPermutationFoundInDict = lookupWordsInDict (convertListOfCharsToEveryPermutation handWithFullInfo) st.dict
            //Her skal logic så være at 'A' er fundet fra listofPossiblePlayCoordinates
            //Derefter skal front/back vælges alt efter:
            //FRONT: DOWN RIGHT
            //BACK: UP LEFT
            let everyPermutationFoundInDictFront = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced 'A' Front handWithFullInfo) st.dict
            let everyPermutationFoundInDictBack = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced 'A' Back handWithFullInfo) st.dict
            let everyPermutationFoundInDictDontAdd = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced 'A' DontAdd handWithFullInfo) st.dict
            forcePrint (sprintf "Every permutation found in dict Front: %A\n" everyPermutationFoundInDictFront)
            forcePrint (sprintf "Every permutation found in dict Back: %A\n" everyPermutationFoundInDictBack)
            forcePrint (sprintf "Every permutation found in dict DontAdd: %A\n" everyPermutationFoundInDictDontAdd)
            
            //Longest word from every permutation
            let longestWordFront = getLongestWordFromResult everyPermutationFoundInDictFront
            let longestWordBack = getLongestWordFromResult everyPermutationFoundInDictBack
            let longestWordDontAdd = getLongestWordFromResult everyPermutationFoundInDictDontAdd
            forcePrint (sprintf "Longest word Front: %A\n" longestWordFront)
            forcePrint (sprintf "Longest word Back: %A\n" longestWordBack)
            forcePrint (sprintf "Longest word DontAdd: %A\n" longestWordDontAdd)

            let listOfPossiblePlayCoords = generateListOfPossibleStartCoords
            //forcePrint (sprintf "listOfPossibleStartCoords: %A\n" listOfPossiblePlayCoords)

            if st.playerTurn = 1u && st.tilesAlreadyPlayed.Count = 0 then
                forcePrint "Ready to do first move\n"
                checkAllDirectionsAndPlay longestWordDontAdd (0,0) handWithFullInfo
            else
                forcePrint "I am not first player\n"
            
            let input = System.Console.ReadLine()
            let move = RegEx.parseMove input

            send cstream (SMPlay move)

            let msg = recv cstream
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                forcePrint "CMPlaySuccess prints: \n"
                //Move made: 0 0 1A1 0 1 20T1
                //forcePrint (sprintf "MS: %A\n" ms) //[((0, 0), (1u, ('A', 1))); ((0, 1), (20u, ('T', 1)))] <- Tiles played, remove from hand.
                //forcePrint (sprintf "Points: %A\n" points) //2
                //forcePrint (sprintf "NewPieces: %A\n" newPieces) //NewPieces: [(6u, 1u); (14u, 1u)]  <- New tiles gotten in hand

                //SAVE MOVES MADE TO tilesAlreadyPlayed
                let rec addMovesToTilesAlreadyPlayed ms =
                    match ms with
                    | [] -> st.tilesAlreadyPlayed
                    | h :: t -> addMovesToTilesAlreadyPlayed t |> Map.add (fst h) (snd h |> fst)

                let tilesAlreadyPlayed = addMovesToTilesAlreadyPlayed ms
                forcePrint (sprintf "Tiles ALREADY played: %A\n" tilesAlreadyPlayed.Count)

                //REMOVE AND ADD NEW TILES TO HAND
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
                
                let listOfPossibleStartCoords = generateListOfPossibleStartCoords
                forcePrint (sprintf "listOfPossibleStartCoords: %A\n" listOfPossibleStartCoords)
                
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
                let listOfPossibleStartCoords = generateListOfPossibleStartCoords
                forcePrint (sprintf "listOfPossibleStartCoords: %A\n" listOfPossibleStartCoords)
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                forcePrint "CMPlayFailed prints: \n"
                forcePrint (sprintf "pid: %A\n" pid)
                forcePrint (sprintf "MS: %A\n" ms)
                let st' = st // This state needs to be updated
                let listOfPossibleStartCoords = generateListOfPossibleStartCoords
                forcePrint (sprintf "listOfPossibleStartCoords: %A\n" listOfPossibleStartCoords)
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
