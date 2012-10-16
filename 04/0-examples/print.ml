let print_list left sep right print_val l =
  print_string left;
  if l <> [] then (
    print_val (List.hd l);
    List.iter
      (fun x -> print_string sep; print_val x)
      (List.tl l)
  );
  print_string right
