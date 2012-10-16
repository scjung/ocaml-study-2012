let ralign width n =
  let s = string_of_int n in
  let padding = max 0 (width - String.length s) in
  String.make padding ' ' ^ s

let mul x y =
  print_endline (ralign 10 x);
  print_endline (" X " ^ (ralign 7 y));
  print_endline (String.make 11 '-');
  let y = string_of_int y in
  let len = String.length y in
  let rec aux res i =
    if i > len then res
    else (
      let n = int_of_string (String.make 1 y.[len-i]) in
      let xn = x * n in
      let b = int_of_float (10.0 ** float_of_int (i-1)) in
      print_endline (ralign (11 - i) xn);
      aux (res + xn * b) (i + 1)
    )
  in
  let res = aux 0 1 in
  print_endline (String.make 11 '-');
  print_endline (ralign 10 res);
  res

