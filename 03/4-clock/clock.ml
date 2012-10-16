(* 3. ANSI 이스케이프 나열 *******************************************)

let clear () = Printf.printf "\027[2J"

let goto x y =
  if x <= 0 || y <= 0 then invalid_arg "goto";
  Printf.printf "\027[%i;%iH%!" y x

let save () = Printf.printf "\0277%!"

let restore () = Printf.printf "\0278%!"

let show_cursor () = Printf.printf "\027[?25h"

let hide_cursor () = Printf.printf "\027[?25l"


(* 4. 디지털 시계 ****************************************************)

(*
    (x,y)        segment_n 함수는 맨 왼쪽 상단 좌표를 받고 n에 해당하는
     |           줄을 그리거나 (on = true) 지운다 (on = false).
     V_____
     |  1  |
   2 |_____| 4
     |  3  |
   5 |_____| 7
        6
 *)
let horizontal x y on =
  goto x y;
  print_string (if on then "_____" else "     ")

let vertical x y on =
  goto x y; print_string (if on then "|" else " ");
  goto x (y + 1); print_string (if on then "|" else " ")

let segment_1 x y n =
  horizontal (x + 1) y
    (n = 0 || n = 2 || n = 3 || n = 5 || n = 7 || n = 8 || n = 9)

let segment_2 x y n =
  vertical x (y + 1)
    (n = 0 || n = 4 || n = 5 || n = 6 || n = 8 || n = 9)

let segment_3 x y n =
  horizontal (x + 1) (y + 2)
    (n = 2 || n = 3 || n = 4 || n = 5 || n = 6 || n = 8 || n = 9)

let segment_4 x y n =
  vertical (x + 6) (y + 1)
    (n = 0 || n = 1 || n = 2 || n = 3 || n = 4 || n = 7 || n = 8 || n = 9)

let segment_5 x y n =
  vertical x (y + 3)
    (n = 0 || n = 2 || n = 6 || n = 8)

let segment_6 x y n =
  horizontal (x + 1) (y + 4)
    (n = 0 || n = 2 || n = 3 || n = 5 || n = 6 || n = 8)

let segment_7 x y n =
  vertical (x + 6) (y + 3)
    (n = 0 || n = 1 || n = 3 || n = 4 || n = 5 || n = 6 || n = 7 || n = 8 || n = 9)

let segment x y n =
  segment_1 x y n;
  segment_2 x y n;
  segment_3 x y n;
  segment_4 x y n;
  segment_5 x y n;
  segment_6 x y n;
  segment_7 x y n

let hour () =
  (Unix.localtime (Unix.gettimeofday ())).Unix.tm_hour

let min () =
  (Unix.localtime (Unix.gettimeofday ())).Unix.tm_min

let sec () =
  (Unix.localtime (Unix.gettimeofday ())).Unix.tm_sec

let clock () =
  clear ();
  goto 1 10;
  while true do
    save ();
    let h = hour () in
    let m = min () in
    let s = sec () in
    let am = h < 12 in
    let h = if h = 12 then 12 else h mod 12 in
    segment 5 5 (h / 10);
    segment 15 5 (h mod 10);
    segment 30 5 (m / 10);
    segment 40 5 (m mod 10);
    segment 55 5 (s / 10);
    segment 65 5 (s mod 10);
    goto 26 7; print_string "|";
    goto 26 9; print_string "|";
    goto 51 7; print_string "|";
    goto 51 9; print_string "|";
    goto 2 5; print_string (if am then "AM" else "PM");
    restore ();
    Unix.sleep 1
  done
