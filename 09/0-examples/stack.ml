type 'a stack = 'a list ref
exception Empty

let create () = ref []
let is_empty s = !s = []
let push s x = s := x :: !s

let pop s =
  match !s with
  | []      -> raise Empty
  | t :: s0 -> s := s0; t

let top s =
  match !s with
  | []     -> raise Empty
  | t :: _ -> t
