let merge file1 file2 new_file =
  let outch = open_out new_file in
  let append inch =
    try
      while true do
        output_string outch (input_line inch);
        output_string outch "\n" (* don't forget it. *)
      done
    with
    | End_of_file -> close_in inch
  in
  append (open_in file1);
  append (open_in file2);
  close_out outch
