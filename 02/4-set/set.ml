let empty n = false

let add n s =
  fun e ->
    if n = e then true
    else s e

let remove n s =
  fun e ->
    if n = e then false
    else s e

let mem n s = s n

let union s1 s2 =
  fun e -> s1 e || s2 e

let inter s1 s2 =
  fun e -> s1 e && s2 e

let range n1 n2 =
  fun e -> n1 <= e && e <= n2
