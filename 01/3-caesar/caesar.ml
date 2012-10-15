let is_alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let first_of is_upper = Char.code (if is_upper then 'A' else 'a')

let last_of is_upper = Char.code (if is_upper then 'Z' else 'z')

(* 문자를 정해진 수마큼 밀어낸다. 밀어낼 때는 알파벳 범위를 넘어가는 경우를
   주의깊게 고려해야 한다.

   예를 들어 'y'의 코드 121을 오른쪽으로 5 밀어낸 값  126은 다음과 같이 대문자
   코드 영역으로 돌아간다.

      65 66 67 68 ... 121 122 ... 126
       A  B  C  D       y   z      |
       +------->|                  |
       |                           |
       ^---------------------------'
 *)
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

(* 주어진 문자열 내 각 알파벳을 주어진 n 만큼 밀어낸다. n 이 양수인 경우는
   오른쪽으로, 음수인 경우는 왼쪽으로 민다. 즉 shift 1 "B" 의 결과는 "C",
   shift (-1) "B" 의 결과는 "A" 이다. *)
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
