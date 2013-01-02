let () =
  if Array.length Sys.argv > 1 then
    print_endline ("Hello, " ^ Sys.argv.(0))
