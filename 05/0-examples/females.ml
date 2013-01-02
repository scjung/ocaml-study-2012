type student = { name : string; female : bool }

let rec females l =
  match l with
  | [] -> 0
  | { name = _; female = true } :: t -> 1 + females t
  | _ :: t -> females t
