type complex = {
  real : float;
  img : float;
}

let add_complex { real = r1; img = i1 }
                { real = r2; img = i2 } =
  { real = r1 +. r2; img = i1 +. i2 }

let neg_complex = function
  | { real; img } -> { real = -. real; img = -. img }
