type bop = Add | Sub | Mul | Div
type uop = Neg
type exp =
  | Int of int
  | Bop of exp * bop * exp
  | Uop of uop * exp

let parse_string s = Uop (Neg, Int 0) (* it's complicated... *)

let string_of_bop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let string_of_uop = function
  | Neg -> "-"

let rec string_of_exp = function
  | Int n -> string_of_int n
  | Bop (e1, bop, e2) -> string_of_exp e1 ^ string_of_bop bop ^ string_of_exp e2
  | Uop (uop, e) -> string_of_uop uop ^ string_of_exp e
