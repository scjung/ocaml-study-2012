let fast_exp a b =
  let rec aux a b =
    if b = 1 then
      a
    else if b mod 2 = 0 then
      let n = aux a (b / 2) in
      n * n
    else
      a * aux a (b - 1)
  in
  if b = 0 then 1
  else if a < 0 || b < 0 then 0
  else aux a b
