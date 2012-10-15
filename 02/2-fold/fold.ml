let fold init s e f =
  let rec aux x n =
    if n > e then x
    else aux (f x n) (n + 1)
  in
  aux init s

let sum s e = fold 0 s e (+)

let fac n = fold 1 1 n ( * )

let alphabets n = fold "" 1 n (fun s n -> s ^ String.make 1 (Char.chr (64 + n)))
