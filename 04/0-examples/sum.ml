let rec sum l =
  if List.length l = 0 then 0
  else List.hd l + sum (List.tl l)
