type tree =
  | Leaf
  | Tree of tree * int * tree

let height = function
  | Leaf           -> 0
  | Tree (l, _, r) -> max (height l) (height r) + 1

let rec add n tree : tree =
  match tree with
  | Leaf            -> Tree (Leaf, n, Leaf)
  | Tree (l, n0, r) ->
      if n = n0 then
        tree
      else if n < n0 then
        Tree (add n l, n0, r)
      else
        Tree (l, n0, add n r)

let rec mem n tree =
  match tree with
  | Leaf    -> false
  | Tree (l, n0, r) ->
      if n = n0 then true
      else mem n (if n < n0 then l else r)

let rec remove n tree =
  let rec remove_max tree =
    match tree with
    | Leaf   -> failwith "ooops"
    | Tree (l, n0, r) ->
        match r with
        | Leaf   -> (n0, remove n0 tree)
        | Tree _ ->
            let (max, r') = remove_max r in
            (max, Tree (l, n0, r'))
  in
  match tree with
  | Leaf            -> Leaf
  | Tree (l, n0, r) ->
      if n < n0 then Tree (remove n l, n0, r)
      else if n > n0 then Tree (l, n0, remove n r)
      else
        match (l, r) with
        | (Leaf, _) -> r
        | (_, Leaf) -> l
        | (Tree _, Tree _) ->
            let (max, l') = remove_max l in
            Tree (l', max, r)

let random_tree n =
  let rec aux i tree =
    if i = 0 then tree
    else aux (i - 1) (add (Random.int 20) tree)
  in
  aux n Leaf

let new_id =
  let c = ref 0 in fun () -> incr c; !c

let dotty file tree =
  let ch = open_out file in
  let rec aux parent_id tree =
    let id = new_id () in
    match tree with
    | Leaf ->
        Printf.fprintf ch "%d [shape=invtriangle,label=\"\"];\n" id;
        if parent_id <> 0 then
          Printf.fprintf ch "%d -> %d;\n" parent_id id

    | Tree (left, n, right) ->
        Printf.fprintf ch "%d [shape=circle,label=\"%d\"];\n"
          id n;
        if parent_id <> 0 then
          Printf.fprintf ch "%d -> %d;\n" parent_id id;
        aux id left;
        aux id right
  in
  Printf.fprintf ch "digraph G {\n";
  aux 0 tree;
  Printf.fprintf ch "}\n";
  close_out ch
