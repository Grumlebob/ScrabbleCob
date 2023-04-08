module DictionaryGaddag

    open System.Collections.Generic
    
    //Using Dictionary because of the benifit of mutability, compared to Map.
    type Dict = IsCompleteWordAndChildNodes of (bool * Dictionary<char, Dict>)
    
    let empty () =
        IsCompleteWordAndChildNodes(false, Dictionary<char, Dict>())
    
    let delimiter = '#'
    
    let insert (word: string) (dict: Dict) : Dict =
        let rec loop (word: char list) (dict: Dict) =
            match word with
            | [] ->
                match dict with
                | IsCompleteWordAndChildNodes (_, childNodes) -> IsCompleteWordAndChildNodes(true, childNodes)
            | head :: tail ->
                match dict with
                | IsCompleteWordAndChildNodes (isComplete, childNodes) ->
                    match childNodes.TryGetValue(head) with
                    | true, childNode -> childNodes.[head] <- loop tail childNode
                    | _ -> childNodes.Add(head, loop tail (empty ()))
    
                    IsCompleteWordAndChildNodes(isComplete, childNodes)
    
        //Convert a Word to the n GADDDAG Variants, n being the lenght of the word.
        let createListOfGaddagWordVariants (word: string) : string list =
            let upperWord = word.ToUpperInvariant()
            let sb = System.Text.StringBuilder()
    
            [ for i in 1 .. upperWord.Length do
                  let reversedStart = Array.init i (fun j -> upperWord.[i - j - 1])
                  let rest = upperWord.[i..]
                  sb.Clear() |> ignore
                  sb.Append(reversedStart, 0, reversedStart.Length) |> ignore
                  sb.Append(delimiter) |> ignore
                  sb.Append(rest) |> ignore
                  yield sb.ToString() ]
            |> List.rev
    
        let wordVariants = createListOfGaddagWordVariants word
        //Use a mutable dictionary to avoid copying the dictionary for each word variant.
        let mutable mutableDict = dict
        //Insert every variant.
        for wordVariant in wordVariants do
            loop (wordVariant.ToCharArray() |> List.ofArray) mutableDict |> ignore
    
        mutableDict
    
    let step (char: char) (dict: Dict) : (bool * Dict) option =
        //Simply, if we can traverse down the tree, return the next node otherwise None.
        match dict with
        | IsCompleteWordAndChildNodes (_, childNodes) ->
            match childNodes.TryGetValue char with
            | false, _ -> None
            | true, childNode ->
                match childNode with
                | IsCompleteWordAndChildNodes (isComplete, _) -> Some(isComplete, childNode)
    
    //Just use the step function, with the delimiter as the char.
    let reverse (dict: Dict) : (bool * Dict) option = step delimiter dict
    
    //We always attempt to do a step down the tree if we are able to.
    //Since words are stored in reverse, we need to reverse the word we are looking up.
    //The end of the word will have a delimiter, so at the end if we can't step anymore, we need to do a reverse
    let lookup (word: string) (dict: Dict) : bool =
        let changeCase = word.ToUpperInvariant()
        let reversedWord = List.rev (changeCase.ToCharArray() |> List.ofArray)
    
        let rec loop (word: list<char>) (dict: Dict) : bool =
            match word with
            | [] ->
                match reverse dict with
                | None -> false
                | Some (reverseComplete, _) -> reverseComplete
            | head :: tail ->
                let stepResult = step head dict
    
                match stepResult with
                | None ->
                    let reverseResult = reverse dict
    
                    match reverseResult with
                    | None -> false
                    | Some (_, childNodes) -> loop word childNodes
                | Some (_, childNodes) -> loop tail childNodes
    
        loop reversedWord dict