let sum_avg l =
  let sum = List.fold_left (+) 0 l in
  let avg =
    float_of_int sum /. float_of_int (List.length l)
  in
  (sum, avg)
