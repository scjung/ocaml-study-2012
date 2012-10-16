let winner l =
  let (winner, _) =
    List.fold_left (fun (winner, max) (name, score) ->
      if score > max then (name, score)
      else (winner, max)
    ) (List.hd l) (List.tl l)
  in
  winner
