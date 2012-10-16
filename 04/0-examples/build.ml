let build f n =
  let rec aux i =
    if i >= n then
      []
    else
      f i :: aux (i + 1)
  in
  aux 0
