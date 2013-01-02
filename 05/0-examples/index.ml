let index l e =
  let rec aux i l =
    match l with
    | []                -> raise Not_found
    | h :: t when e = h -> i
    | _ :: t            -> aux (i + 1) t
  in
  aux 0 l
