let is_alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

(* 밀어낸 문자 코드가 알파벳 코드가 되도록 돌린다.

   예를 들어 'y'의 코드 121을 오른쪽으로 5 밀어낸 값  126은 다음과 같이 대문자
   코드 영역으로 돌아간다.

      65 66 67 68 ... 121 122 ... 126
       A  B  C  D       y   z      |
       +------->|                  |
       |                           |
       ^---------------------------'
 *)
let round code =
  let code_A = Char.code 'A' in
  let code_Z = Char.code 'Z' in
  let code_a = Char.code 'a' in
  let code_z = Char.code 'z' in
  if code < code_A then
    code_z - (code_A - code) + 1
  else if code_Z < code && code < code_a then
    code_a + (code - code_Z) - 1
  else if code_z < code then
    code_A + (code - code_z) - 1
  else
    code

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
          Char.chr (round (Char.code plain + n))
        else
          plain
      in
      String.make 1 encrypted ^ aux (i + 1)
  in
  if s = "" then "" else aux 0

let encrypt n s = shift n s

let decrypt n s = shift (-n) s
