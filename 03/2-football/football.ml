exception Data_error of string

(* 쉼표로 분리된 문자열에서 맨 처음 것과 나머지를 분리해서 주어진 함수에
   적용한다. *)
let use_item str f =
  try
    let i = String.index str ',' in
    f (String.sub str 0 i)
      (String.sub str (i + 1) (String.length str - i - 1))
  with
  | Not_found -> raise (Data_error ("No comma: " ^ str))

let football file team =
  let inch = open_in file in
  let rec read played won lost drawn gs ga pts =
    try
      let line = input_line inch in
      use_item line (fun date line ->
        use_item line (fun team1 line ->
          use_item line (fun score1 line ->
            use_item line (fun team2 score2 ->
              let score1 =
                try
                  int_of_string score1
                with
                | Failure m -> raise (Data_error ("Not a score: " ^ score1))
              in
              let score2 =
                try
                  int_of_string score2
                with
                | Failure m -> raise (Data_error ("Not a score: " ^ score2))
              in
              if team1 = team then
                if score1 > score2 then
                  read (played + 1) (won + 1) lost drawn (gs + score1) (ga + score2) (pts + 3)
                else if score1 < score2 then
                  read (played + 1) won (lost + 1) drawn (gs + score1) (ga + score2) (pts + 0)
                else
                  read (played + 1) won lost (drawn + 1) (gs + score1) (ga + score2) (pts + 1)

              else if team2 = team then
                if score1 < score2 then
                  read (played + 1) (won + 1) lost drawn (gs + score2) (ga + score1) (pts + 3)
                else if score1 > score2 then
                  read (played + 1) won (lost + 1) drawn (gs + score2) (ga + score1) (pts + 0)
                else
                  read (played + 1) won lost (drawn + 1) (gs + score2) (ga + score1) (pts + 1)

              else
                read played won lost drawn gs ga pts
            ))))
    with
    | End_of_file ->
        print_endline team;
        Printf.printf "    Played:        %3d\n" played;
        Printf.printf "    Won:           %3d\n" won;
        Printf.printf "    Lost:          %3d\n" lost;
        Printf.printf "    Drawn:         %3d\n" drawn;
        Printf.printf "    Goals Scored:  %3d\n" gs;
        Printf.printf "    Goals Allowed: %3d\n" ga;
        Printf.printf "    Points:        %3d\n" pts
  in
  read 0 0 0 0 0 0 0
