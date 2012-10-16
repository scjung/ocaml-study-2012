let clear () = Printf.printf "\027[2J"

(* "%!" 를 사용하면 버퍼가 즉시 비워진다 (flush). *)
let goto x y =
  if x <= 0 || y <= 0 then invalid_arg "goto";
  Printf.printf "\027[%i;%iH%!" y x

let save () = Printf.printf "\0277%!"

let restore () = Printf.printf "\0278%!"

let show_cursor () = Printf.printf "\027[?25h"

let hide_cursor () = Printf.printf "\027[?25l"
