let prompt m = print_string m; read_line ()
let remember () =
  let rec input l =
    let k = prompt "? " in
    if k = "" then l else input ((k, prompt (k ^ "? ")) :: l)
  in
  let rec ask l =
    let k = prompt "Ask me? " in
    if k = "" then () else (
      print_endline (
        try List.assoc k l
        with Not_found -> "I don't know that."
      );
      ask l
    )
  in
  ask (input [])
