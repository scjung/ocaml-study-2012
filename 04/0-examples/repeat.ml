let rec repeat n x =
  if n <= 0 then
    []
  else
    x :: repeat (n - 1) x
