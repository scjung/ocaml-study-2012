let swap x y =
  let tmp = !x in
  x := !y; y := tmp
