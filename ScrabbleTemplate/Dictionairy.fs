// Insert your Dictionary.fsi file here. All modules must be internal.

module Dictionary
    type Dict =
        | Node of bool * Map<char, Dict>

    let empty () = Node (false, Map.empty)

    let insert (s: string) dict =
        let rec aux =
            function
            | [], Node (_, m)      -> Node (true, m)
            | x :: xs, Node (b, m) ->
                match m.TryFind(x) with
                | None   -> Node (b, m.Add(x, aux (xs, empty ())))
                | Some v -> Node (b, m.Add(x, aux (xs, v)))

        aux (Seq.toList s, dict)

    let lookup (s: string) (dict : Dict) =
        let rec aux =
            function
            | [], Node (b, _)      -> b
            | x :: xs, Node (_, m) ->
                match m.TryFind(x) with
                | None   -> false
                | Some v -> aux (xs, v)

        aux (Seq.toList s, dict)

    let step c dict =
        let aux =
            function
            | c1, Node (b, m) ->
                match m.TryFind(c1) with
                | None                -> None
                | Some (Node(bo, m1)) -> Some (bo, Node(bo, m1))
        aux (c, dict)
