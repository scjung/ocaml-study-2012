type coin =
  | Head
  | Tail

let toss () =
  if Random.int 2 = 0 then
    Head
  else
    Tail

let choose coin h t =
  match coin with
  | Head -> h
  | Tail -> t
