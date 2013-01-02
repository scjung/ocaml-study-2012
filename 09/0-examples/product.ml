let row m = Array.length m
let col m = Array.length m.(0)
let product a b =
  let (row_a, col_a) = (row a, col a) in
  let (row_b, col_b) = (row b, col b) in
  if col_a <> row_b then invalid_arg "product";
  let ab = Array.make_matrix row_a col_b 0 in
  for i = 0 to row_a - 1 do
    for j = 0 to col_b - 1 do
      for k = 0 to col_a - 1 do
        ab.(i).(j) <- ab.(i).(j) + a.(i).(k) * b.(k).(j)
      done
    done
  done;
  ab
