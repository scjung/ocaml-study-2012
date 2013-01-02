let take =
  let rec aux l' l n =
    match (l, n) with
    | ([],     _) -> List.rev l'
    | (_,      0) -> List.rev l'
    | (h :: t, _) -> aux (h :: l') t (n - 1)
  in
  fun n l ->
    if n <= 0 then []
    else aux [] l n
