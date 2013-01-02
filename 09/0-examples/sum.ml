let sum s e =
  let r = ref 0 in
  for i = s to e do
    r := i + !r
  done;
  !r
