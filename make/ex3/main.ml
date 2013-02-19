let () =
  while true do
    print_string "? ";
    let s = read_line () in
    print_string "= ";
    print_int (Eval.eval (Ast.parse_string s));
    print_newline ()
  done
