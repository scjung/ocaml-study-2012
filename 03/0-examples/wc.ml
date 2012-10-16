let wc file =
  let ch = open_in file in
  let rec count chars lines =
    try
      let line = input_line ch in
      count
        (chars + String.length line + 1) (* count \n *)
        (lines + 1)
    with
    | End_of_file ->
        close_in ch;
        Printf.printf "chars : %5d\n" chars;
        Printf.printf "lines : %5d\n" lines
  in
  count 0 0
