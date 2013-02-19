open Ast

let evaluator_of_bop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )

let evaluator_of_uop = function
  | Neg -> ( ~- )

let rec eval = function
  | Int n -> n
  | Bop (e1, bop, e2) -> (evaluator_of_bop bop) (eval e1) (eval e2)
  | Uop (uop, e) -> (evaluator_of_uop uop) (eval e)
