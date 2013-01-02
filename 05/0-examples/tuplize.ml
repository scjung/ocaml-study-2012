let pair_of_list l =
  match l with
  | [] | [_] -> invalid_arg "insufficient values"
  | [x; y] -> (x, y)
  | _ -> invalid_arg "too many values"

let triple_of_list l =
  match l with
  | [] | [_] | [_; _] -> invalid_arg "insufficient values"
  | [x; y; z] -> (x, y, z)
  | _ -> invalid_arg "too many values"
