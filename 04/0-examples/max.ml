let max_of_list l =
  List.fold_left max (List.hd l) (List.tl l)

let min_of_list l =
  List.fold_left min (List.hd l) (List.tl l)
