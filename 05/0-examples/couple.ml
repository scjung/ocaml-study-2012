let rec couple l =
  match l with
  | [] | [_]    -> []
  | x :: y :: t -> (x, y) :: couple (y :: t)
