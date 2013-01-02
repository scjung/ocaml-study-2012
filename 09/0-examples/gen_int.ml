let gen_int =
  let n = ref 0 in
  fun () -> incr n; !n
