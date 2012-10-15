let rec roman_of_int n =
  if n = 0 then
    ""
  else if n >= 100 then
    String.make (n / 100) 'C' ^ roman_of_int (n mod 100)
  else if n >= 90 then
    "XC" ^ roman_of_int (n - 90)
  else if n >= 50 then
    String.make (n / 50) 'L' ^ roman_of_int (n mod 50)
  else if n >= 40 then
    "XL" ^ roman_of_int (n - 40)
  else if n >= 10 then
    String.make (n / 10) 'X' ^ roman_of_int (n mod 10)
  else if n >= 9 then
    "IX" ^ roman_of_int (n - 9)
  else if n >= 5 then
    String.make (n / 5) 'V' ^ roman_of_int (n mod 5)
  else if n = 4 then
    "IV"
  else
    String.make n 'I'

(* 로마 수를 정수로 바꿀 때는 문자를 하나 혹은 두 개 단위로 끊어서 보면서, 가장
   큰 단위부터 어떠한 것이 가능한지 찾는다. 즉 단위별로 로마 수를 해석하는
   함수가 있어야 하며, 이 함수는 이제 봐야할 문자의 인덱스와 이제까지 알아낸
   정수를 인자로 받는다. 덧붙여 I 처럼 여러 번 반복이 가능한 경우에는 이를
   해석하는 함수는 현재까지 반복된 회수 또한 인자로 받아야 한다.

   만일 지금 고려하는 단위의 수가 더 이상 반복될 수 없거나, 다른 문자가 오는
   경우에는 더 작은 단위를 고려한다. 이 때 바로 밑의 단위가 불가능 할 수 있음에
   유의한다. 예를 들어 9를 의미하는 IX 의 경우 밑의 단위인 I, IV, V 모두
   불가능하다. I가 오는 경우, IXI 는 10 이므로 이는 X 로 표현되어야 하기
   때문이다. *)
let int_of_roman s =
  let l = String.length s in
  (* 문자 두 개를 검토 *)
  let two_letters i c1 c2 = i < l - 1 && s.[i] = c1 && s.[i + 1] = c2 in

  (* I = 1. 반복 가능. *)
  let rec aux_I rpt i n =
    if i = l then n
    else if s.[i] = 'I' then
      if rpt >= 4 then 0    (* IIII = IV *)
      else aux_I (rpt + 1) (i + 1) (n + 1)
    else 0
  in

  (* IV = 4 *)
  let aux_IV i n =
    if i = l then n
    else if two_letters i 'I' 'V' then
      (* IV 이후에는 올 것이 없다. IVI는 V로 표현되어야 함. *)
      if i + 2 = l then n + 4 else 0
    else aux_I 1 i n
  in

  (* V = 5 *)
  let aux_V i n =
    if i = l then n
    (* V는 반복 불가. VV = 10 = X.
       V 다음에는 IV가 올 수 없음. VIV = 9 = IX. *)
    else if s.[i] = 'V' then aux_I 1 (i + 1) (n + 5)
    else aux_IV i n
  in

  (* IX = 9 *)
  let aux_IX i n =
    if i = l then n
    else if two_letters i 'I' 'X' then
      (* IX 다음에는 어떠한 것도 올 수 없음.
         IXV = 14 = XIV, IXIV = 13 = XIII, IXI = 10 = X *)
      if i + 2 = l then n + 9 else 0
    else
      aux_V i n
  in

  (* X = 10. 반복 가능. *)
  let rec aux_X rpt i n =
    if i = l then n
    else if s.[i] = 'X' then
      if rpt >= 4 then 0 (* XXXX = 40 = XL *)
      else aux_X (rpt + 1) (i + 1) (n + 10)
    else aux_IX i n
  in

  (* XL = 40 *)
  let aux_XL i n =
    if i = l then n
    else if two_letters i 'X' 'L' then
      (* XL 다음에는 X가 올 수 없음. XLX = 50 = L *)
      aux_IX (i + 2) (n + 40)
    else
      aux_X 1 i n
  in

  (* L = 50 *)
  let aux_L i n =
    if i = l then n
    else if s.[i] = 'L' then
      (* L은 반복 불가. LL = 100 = C *)
      aux_X 1 (i + 1) (n + 50)
    else
      aux_XL i n
  in

  (* XC = 90 *)
  let rec aux_XC i n =
    if i = l then n
    else if two_letters i 'X' 'C' then
      (* XC 다음에는 X, L 이 올 수 없음
         XCX = 100 = C, XCL = 140 = CXXXX *)
      aux_IX (i + 2) (n + 90)
    else
      aux_L i n
  in

  (* C = 100 *)
  let rec aux_C i n =
    if i = l then n
    else if s.[i] = 'C' then aux_C (i + 1) (n + 100)
    else aux_XC i n
  in

  aux_C 0 0
