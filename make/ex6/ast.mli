type bop = Add | Sub | Mul | Div
type uop = Neg
type exp =
  | Int of int
  | Bop of exp * bop * exp
  | Uop of uop * exp

val parse_string : string -> exp
val string_of_exp : exp -> string
