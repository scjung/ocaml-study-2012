let cat filename =
  let inch = open_in filename in
  try
    while true do
      print_string (input_line inch ^ "\n")
    done
  with End_of_file -> close_in inch
