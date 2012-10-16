let even_odd () =
  while true do
    let s = read_line () in
    let n = int_of_string s in
    print_string (
      if n mod 2 = 0 then "even\n" else "odd\n"
    )
  done
    (* To stop the program, press Ctrl+C *)
