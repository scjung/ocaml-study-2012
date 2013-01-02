let rec last l =
  match l with
  | []     -> invalid_arg "empty"
  | [h]    -> h
  | _ :: t -> last t
