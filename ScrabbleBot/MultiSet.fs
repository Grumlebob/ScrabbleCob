module MultiSet 
    
    type MultiSet<'T when 'T: comparison> = MultiSet of Map<'T, uint32>
    let empty = MultiSet Map.empty
    
    let isEmpty (m: MultiSet<'T>) : bool =
        match m with
        | MultiSet m -> m |> Map.isEmpty
    
    let size (m: MultiSet<'T>) : uint32 =
        match m with
        | MultiSet m -> Map.fold (fun acc _ value -> acc+value) 0u m
                     
    let contains (element: 'T) (m: MultiSet<'T>) : bool =
        match m with
        | MultiSet m -> m |> Map.containsKey element
    
    let numItems (element: 'T) (m: MultiSet<'T>) : uint32 =
        match m with
        | MultiSet m ->
            match m.TryFind element with
            | Some(num) -> num
            | None -> 0u
    
    let add (element: 'T) (timesToAddElement: uint32) (multiset: MultiSet<'T>) : MultiSet<'T> =
        let newNumOfItems = numItems element multiset + timesToAddElement
        match multiset with
        | MultiSet m -> MultiSet(m |> Map.add element newNumOfItems)
        
    let addSingle (element: 'T) (m: MultiSet<'T>) : MultiSet<'T> = m |> add element 1u
    
    let remove (element: 'T) (timesToRemoveElement: uint32) (m: MultiSet<'T>) : MultiSet<'T> =
        //Cast to int, to handle overflow. Otherwise 1u - 10u wil be 79797987494
        let newTotalItems = (numItems element m |> int) - (timesToRemoveElement |> int)
        match m with
        | MultiSet m ->
            //If total is greater, overwrite with new total
            if newTotalItems > 0 then
                MultiSet(m |> Map.add element (newTotalItems |> uint32))
            else
                //If total is 0 or less, remove entirely from map
                MultiSet(m |> Map.remove element)
    
    let removeSingle (element: 'T) (m: MultiSet<'T>) : MultiSet<'T> = m |> remove element 1u
    
    let fold folder acc (multiset: MultiSet<'T>) =
        match multiset with
        | MultiSet multiset -> Map.fold folder acc multiset
    
    let foldBack folder (multiset: MultiSet<'T>) acc =
        match multiset with
        | MultiSet multiset -> Map.foldBack folder multiset acc
    
    let ofList (list: 'T list) : MultiSet<'T> =
        list |> List.fold (fun acc element -> (addSingle element acc)) empty
    
    let toList (multiset: MultiSet<'T>) : 'T list =
        fold (fun acc key value -> (List.replicate (value |> int) key) @ acc) [] multiset |> List.rev
        
    let map mapper (multiset: MultiSet<'A>) : MultiSet<'B> =
        fold (fun acc key value -> (add (mapper(key)) value acc)) empty multiset
        
    let union (multiset1: MultiSet<'T>) (multiset2: MultiSet<'T>) : MultiSet<'T> =
        //Acc is multiset1 and key value is multiset2
        fold (fun ms1 key value ->
            //As add function doesn't overwrite value, we need to find highest value and minus current value
            //E.g ms1: 6 and ms2: 4, we want to add 2 to ms2. If we mistakenly just added 6, we would end up with 10
            let highestValue = max value (numItems key multiset1)
            let currentValue = numItems key ms1
            //Add the difference to our accumulator ms1
            ((add key (highestValue-currentValue)) ms1)) multiset1 multiset2
    
    let sum (multiset1: MultiSet<'T>) (multiset2: MultiSet<'T>) : MultiSet<'T> =
        //Same as union, without any maxing, just add the values
        fold (fun ms1 key value -> add key value ms1) multiset1 multiset2
    
    let subtract (multiset1: MultiSet<'T>) (multiset2: MultiSet<'T>) : MultiSet<'T> =
        //Same as sum, just remove instead of add
        fold (fun ms1 key value -> remove key value ms1) multiset1 multiset2
                                      
    let intersection (multiset1: MultiSet<'T>) (multiset2: MultiSet<'T>) : MultiSet<'T> =
        //Acc is empty and key value is multiset2
        fold (fun acc key _ ->            
            let ms1Value = numItems key multiset1
            let ms2Value = numItems key multiset2
            //Pick lowest value if both has contents (key is not empty in both sets)
            if (ms1Value <> 0u && ms2Value <> 0u) then
                if ms1Value > ms2Value then
                    add key ms2Value acc
                else
                    add key ms1Value acc
            else
                //Else don't add, and just send accumulator through
                acc) empty multiset2    

    