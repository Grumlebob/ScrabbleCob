namespace GrumboBot

open System.Threading
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint


type Direction =
    | Right
    | Down
    | Left
    | Up

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

        let rec aux (st: State.state) (myTurn: bool) =
            
            if myTurn then
                //Thread.Sleep(2000)
                forcePrint "HAND PRINT:\n"
                Print.printHand handIdToTile (State.hand st)
                
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
                
                //TODO istedet for stringlist kunne den returnere (string*sumOfPoints) list
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
                //TODO CHANGE TIL AT DEN KIGGER PÅ POINTS
                //Behold denne metode, men lav også en der kigger på højeste point.
                
                let getLongestWordFromResult (resultOfLookupEveryStringInPermutationTest:string list) : string=
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
                    let oneCoordBehindTarget =
                        match direction with
                        | Right -> (fst coord - 1, snd coord)
                        | Left -> (fst coord + 1, snd coord)
                        | Up -> (fst coord, snd coord + 1)
                        | Down -> (fst coord, snd coord - 1)
                    
                    let moveOneCoord = getNewCoord coord direction
                    
                    let mainCoordsToCheck = 
                        match direction with
                        | Right -> List.init lenghtToCheck (fun x -> (fst moveOneCoord + x, snd moveOneCoord))
                        | Left -> List.init lenghtToCheck (fun x -> (fst moveOneCoord - x, snd moveOneCoord))
                        | Up -> List.init lenghtToCheck (fun x -> (fst moveOneCoord, snd moveOneCoord - x))
                        | Down -> List.init lenghtToCheck (fun x -> (fst moveOneCoord, snd moveOneCoord + x))
                    //Ie. if going right we are checking 1 up and 1 down from the main coords, to guarantee that we can freely place the word
                    let secondaryCoordsToCheck =
                        match direction with
                        | Right -> List.map (fun x -> (fst x, snd x + 1)) mainCoordsToCheck
                        | Left -> List.map (fun x -> (fst x, snd x - 1)) mainCoordsToCheck
                        | Up -> List.map (fun x -> (fst x - 1, snd x)) mainCoordsToCheck
                        | Down -> List.map (fun x -> (fst x + 1, snd x)) mainCoordsToCheck
                    
                    let tertiaryCoordsToCheck =
                        match direction with
                        | Right -> List.map (fun x -> (fst x, snd x - 1)) mainCoordsToCheck
                        | Left -> List.map (fun x -> (fst x, snd x + 1)) mainCoordsToCheck
                        | Up -> List.map (fun x -> (fst x + 1, snd x)) mainCoordsToCheck
                        | Down -> List.map (fun x -> (fst x - 1, snd x)) mainCoordsToCheck
                                        
                    let allCoordsToCheck =  mainCoordsToCheck @ secondaryCoordsToCheck @ tertiaryCoordsToCheck @ [oneCoordBehindTarget]

                    let nextTileIsFree = List.forall (fun x -> Map.tryFind x st.tilesAlreadyPlayed = None) allCoordsToCheck
                    (nextTileIsFree,direction)
                    
         
                let PlayFirstTurnMove (longestWord:string) (startCoord: coord) (handWithFullInfo:(char * uint32 * int) list) =
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
                    
                    let move = playLongestWord longestWord startCoord handWithFullInfo Right
                    send cstream (SMPlay move)
    
                let playMove (longestWord:string) (startCoord: coord) (handWithFullInfo:(char * uint32 * int) list) (direction: Direction) : (coord * (uint32 * (char * int))) list =
                    let skipFirstCoord = match direction with
                                         | Right -> true
                                         | Down -> true
                                         | Left -> false
                                         | Up -> false
                    let lengthOfWord = longestWord.Length
                    let reverseString (str: string) : string =
                        let mutable reversedStr = ""
                        for i = str.Length - 1 downto 0 do
                            reversedStr <- reversedStr + string(str.[i])
                        reversedStr

                    let rec loop (loopcounter: int) (acc:(coord * (uint32 * (char * int))) list) (chars:char list) (currentCoord:coord) =
                        match chars with
                        | _ when loopcounter = lengthOfWord ->
                            loop (loopcounter-1) acc chars.Tail (getNewCoord currentCoord direction)
                        | [] -> acc
                        | h :: t ->
                            let (char, id, pV) = List.find (fun (c, _, _) -> c = h) handWithFullInfo
                            let newMove = (currentCoord), (id, (char, pV))
                            let newCoord = getNewCoord currentCoord direction
                            loop (loopcounter-1) (newMove :: acc) t newCoord
                    
                    if skipFirstCoord then
                        let charList = List.ofArray (longestWord.ToCharArray())
                        loop lengthOfWord [] charList startCoord
                    else
                        let reversedWord = reverseString longestWord
                        let reversedCharList = List.ofArray (reversedWord.ToCharArray())
                        loop lengthOfWord [] reversedCharList startCoord
                
                let generateListOfPossibleStartCoords =
                    let listOfCoordsFromTilesPlayed = Map.toList st.tilesAlreadyPlayed |> List.map (fun (coord, _) -> coord)
                    let rec generateListOfPossibleStartCoords (lenghtToCheck: int) (coords: coord list) (acc: (coord * Direction) list)  =
                        match coords with
                        | [] -> acc
                        | coord :: t ->
                            let checkCoordRight = checkIfPlacementIsPossible lenghtToCheck coord Right
                            let checkCoordDown = checkIfPlacementIsPossible lenghtToCheck coord Down
                            let checkCoordLeft = checkIfPlacementIsPossible lenghtToCheck coord Left
                            let checkCoordUp = checkIfPlacementIsPossible lenghtToCheck coord Up
                            let allDirections = [checkCoordRight; checkCoordLeft; checkCoordUp; checkCoordDown]
                            let rec checkAllDirections (allDirections:(bool*Direction) list) (acc: (coord * Direction) list)  =
                                match allDirections with
                                | [] ->
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
                
                if st.playerTurn = 1u && st.tilesAlreadyPlayed.Count = 0 then
                    forcePrint "Doing opening move\n"
                    let everyPermutationFoundInDictDontAdd = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced 'A' DontAdd handWithFullInfo) st.dict
                    let longestWordDontAdd = getLongestWordFromResult everyPermutationFoundInDictDontAdd
                    //0,0 er center square, men hvordan trækker jeg den automatisk?
                    PlayFirstTurnMove longestWordDontAdd (0,0) handWithFullInfo
                else
                    //forcePrint "GIK IND I NORMAL MOVE\n"
                    let listOfPossiblePlayCoords = generateListOfPossibleStartCoords
                    
                    let rec checkEveryCoordFindLongestWord (listOfPossiblePlayCoords: (coord * Direction) list) (acc: (string*coord*Direction)) =
                        match listOfPossiblePlayCoords with
                        | [] ->
                            forcePrint "Done checking every coord\n"
                            acc
                        | h :: t ->
                            let (coord, direction) = h
                            let shouldAddToFront =
                                match direction with
                                | Right -> true
                                | Down -> true
                                | _ -> false
                            let convertTileIdToChar (id:uint32) : char =
                                let ab = handIdToTile.[id] |> List.ofSeq
                                match ab with
                                | h::t -> fst h
                                | _ -> failwith "Should never happen"
                            let charToBuildFrom = convertTileIdToChar st.tilesAlreadyPlayed.[coord]
                            if shouldAddToFront then
                                let everyPermutationFoundInDictFront = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced charToBuildFrom Front handWithFullInfo) st.dict
                                let longestWordFront = getLongestWordFromResult everyPermutationFoundInDictFront
                                match acc with
                                | (currentLongest, _, _) ->
                                    if longestWordFront.Length > currentLongest.Length then
                                        checkEveryCoordFindLongestWord t (longestWordFront, coord, direction)
                                    else
                                        checkEveryCoordFindLongestWord t acc                                
                            else
                                let everyPermutationFoundInDictBack = lookupWordsInDict (convertListOfCharsToEveryPermutationWithFixedCharPlaced charToBuildFrom Back handWithFullInfo) st.dict
                                let longestWordBack = getLongestWordFromResult everyPermutationFoundInDictBack
                                match acc with
                                | (currentLongest, _, _) ->
                                    if longestWordBack.Length > currentLongest.Length then
                                        checkEveryCoordFindLongestWord t (longestWordBack, coord, direction)
                                    else
                                        checkEveryCoordFindLongestWord t acc
                    let (bestWord, bestCoord, bestDirection) = checkEveryCoordFindLongestWord listOfPossiblePlayCoords ("", (0,0), Right)
                    if bestWord = "" then
                        let handAsList = MultiSet.toList st.hand
                        forcePrint (sprintf "Hand exchanging: %A\n" handAsList)
                        send cstream (SMChange handAsList)
                    else
                        let move = playMove bestWord bestCoord handWithFullInfo bestDirection
                        send cstream (SMPlay move)
                    
            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                //SAVE MOVES MADE TO tilesAlreadyPlayed
                let rec addMovesToTilesAlreadyPlayed ms =
                    match ms with
                    | [] -> st.tilesAlreadyPlayed
                    | h :: t -> addMovesToTilesAlreadyPlayed t |> Map.add (fst h) (snd h |> fst)

                let tilesAlreadyPlayed = addMovesToTilesAlreadyPlayed ms
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
                
                //False, because next turn is opponent
                aux st' false
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //Add moves ms to tilesAlreadyPlayed
                let rec addMovesToTilesAlreadyPlayed ms =
                    match ms with
                    | [] -> st.tilesAlreadyPlayed
                    | h :: t -> addMovesToTilesAlreadyPlayed t |> Map.add (fst h) (snd h |> fst)

                let newTiles = addMovesToTilesAlreadyPlayed ms
                let st' = { st with tilesAlreadyPlayed = newTiles }
                //TODO LAV MODULUS MAGIC HER, SÅ DET ALTID DEN RIGTIGE PLAYER TUR HER.
                aux st' true
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st' false
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
                //(player % uint32 (st.playerStatus.Length) + 1u = st.playerNumber)
                aux st' false
            
            | RCM (CMChange (pid, numberOfTiles)) ->
                forcePrint "CMChange prints: \n"
                let st' = st // This state needs to be updated
                aux st' false
            | RCM (CMChangeSuccess (newTiles)) ->
                forcePrint "CMChangeSuccess prints: \n"
                forcePrint (sprintf "NewTiles To Have: %A\n" newTiles)
                let rec addNewGainedTiles (newPieces: (uint32 * uint32) list) (hand: MultiSet.MultiSet<uint32>) =
                    match newPieces with
                    | [] -> hand
                    | h :: t -> addNewGainedTiles t (MultiSet.addSingle (fst h) hand)
                //Remove all pieces from hand and change to new ones.
                let newHand = MultiSet.empty
                let newHand = addNewGainedTiles newTiles newHand
                let st' = { st with hand = newHand }
                aux st' false
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st false
        
        if st.playerTurn = 1u && st.playerNumber = 1u  && st.tilesAlreadyPlayed.Count = 0 then
            aux st true
        else 
            aux st false

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
        //TODO HAR FJERNET HER
        //let getFirstElement tile = Set.toList tile |> (fun x -> x.Head)
        //let gameStartTiles = Map.map (fun id tileTuple -> getFirstElement tileTuple) tiles

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber playerTurn List.empty handSet Map.empty)
