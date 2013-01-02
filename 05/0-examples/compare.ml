let rec list_compare l1 l2 =
  match (l1, l2) with
  | ([], []) -> 0
  | (_, [])  -> 1   (* l1 has more values than l2. *)
  | ([], _)  -> -1  (* l2 has more values than l1. *)
  | (h1 :: t1, h2 :: t2) ->
      match compare h1 h2 with
      | 0 -> list_compare t1 t2
      | r -> r
