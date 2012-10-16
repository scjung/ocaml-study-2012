let pyramid () =
  print_string "How tall do you want? ";
  let height = read_int () in  (* Pervasives.read_int *)
  let rec aux i =
    if i < height then (
      print_string (String.make (height - i) ' ');
      print_string "/";
      print_string (String.make (i * 2) '-');
      print_string "\\\n";
      aux (i + 1)
    )
  in
  aux 0;
  print_string (String.make ((i + 1) * 2) '.' ^ "\n")
