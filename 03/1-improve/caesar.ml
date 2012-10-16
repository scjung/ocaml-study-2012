let is_alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let first_of is_upper = Char.code (if is_upper then 'A' else 'a')

let last_of is_upper = Char.code (if is_upper then 'Z' else 'z')

let shift_char n c =
  let code = Char.code c in
  let upper = 'A' <= c && c <= 'Z' in
  let idx = code - first_of upper in
  let shifted_code =
    if n > 0 then
      let overflow = n - (26 - idx - 1) in
      if overflow > 0 then
        first_of (not upper) + overflow - 1
      else
        code + n
    else
      let underflow = idx + n in
      if underflow < 0 then
        last_of (not upper) + underflow + 1
      else
        code + n
  in
  Char.chr shifted_code

let shift n s =
  let rec aux i =
    if String.length s <= i then
      ""
    else
      let plain = s.[i] in
      let encrypted =
        if is_alpha plain then
          shift_char n plain
        else
          plain
      in
      String.make 1 encrypted ^ aux (i + 1)
  in
  if s = "" then "" else aux 0

let encrypt n s = shift n s

let decrypt n s = shift (-n) s

(* 입력 채널에서 받은 문자열 줄을 함수에 적용하고, 그 결과를 출력 채널에
   써넣는다. UNIX의 파이프(pipe)와 비슷한 기능을 수행하는 함수이다. *)
let pipe inch f outch =
  try
    while true do
      output_string outch (f (input_line inch));
      output_string outch "\n"
    done
  with
  | End_of_file -> ()

let pipe_file infile f outfile =
  let inch = open_in infile in
  let outch = open_out outfile in
  try
    pipe inch f outch;
    close_in inch;
    close_out outch
  with
  | e ->
      close_in inch;
      close_out outch;
      raise e

(* 파일의 내용을 암호화하여 파일에 저장한다. *)
let encrypt_file n infile outfile = pipe_file infile (encrypt n) outfile

(* 파일의 암호를 풀어서 파일에 저장한다. *)
let decrypt_file n infile outfile = pipe_file infile (decrypt n) outfile
