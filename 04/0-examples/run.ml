let rec run l =
  if List.length l = 0 then ()
  else (
    (List.hd l) ();
    run (List.tl l)
  )
