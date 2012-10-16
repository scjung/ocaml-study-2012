let print_tuple2 f1 f2 (x1, x2) =
  print_string "(";
  f1 x1; print_string ", "; f2 x2;
  print_string ")"

let print_tuple3 f1 f2 f3 (x1, x2, x3) =
  print_string "(";
  f1 x1; print_string ", "; f2 x2; print_string ", "; f3 x3;
  print_string ")"
