let replaced orig_s orig_c new_c =
  let s = String.copy orig_s in
  let lidx = String.length s - 1 in
  for i = 0 to lidx do
    if s.[i] = orig_c then s.[i] <- new_c
  done;
  s
