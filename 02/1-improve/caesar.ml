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

(* 문자열 내 각 문자에 주어진 함수 f를 적용하고 얻어낸 문자로 새 문자열을
   만든다. *)
let string_map f s =
  let l = String.length s in
  let rec aux i =
    if l <= i then
      ""
    else
      (String.make 1 (f s.[i])) ^ (aux (i + 1))
  in
  aux 0

(* 문자열 내 각 문자를 p에 적용했을 때 결과가 true인 경우는 그 문자를 함수 f에
   적용하고 결과 문자를 취하고, false인 경우에는 기존 문자를 그대로 유지하여 새
   문자열을 만들어 낸다. *)
let string_map_if p f s = string_map (fun c -> if p c then f c else c) s

(* string_map_if 를 사용해서 shift 구현 *)
let shift n = string_map_if is_alpha (shift_char n)

let encrypt n s = shift n s

let decrypt n s = shift (-n) s
