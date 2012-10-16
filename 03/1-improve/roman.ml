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

(* 잘못된 형태의 로마 수가 사용될 때 발생하는 예외상황.  예전에는 잘못된 형태의
   로마 수가 사용될 때 0을 대신 내주었지만, 이제는 이 예외상황을 발생시킨다. *)
exception Invalid_roman

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
    else
      raise Invalid_roman
  in

  (* IV = 4 *)
  let aux_IV i n =
    if i = l then n
    else if two_letters i 'I' 'V' then
      (* IV 이후에는 올 것이 없다. IVI는 V로 표현되어야 함. *)
      if i + 2 = l then n + 4 else raise Invalid_roman
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
      if i + 2 = l then n + 9 else raise Invalid_roman
    else
      aux_V i n
  in

  (* X = 10. 반복 가능. *)
  let rec aux_X rpt i n =
    if i = l then n
    else if s.[i] = 'X' then
      if rpt >= 4 then raise Invalid_roman (* XXXX = 40 = XL *)
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

let rec qna_loop pmt f =
  print_string pmt;
  let q = read_line () in
  if q = "" then
    ()
  else (
    print_endline (f q);
    qna_loop pmt f
  )

let roman () =
  qna_loop "? " (fun q ->
    try
      roman_of_int (int_of_string q)
    with
    | Failure _ -> (* 정수가 아님. 아마도 로마수? *)
        try
          string_of_int (int_of_roman q)
        with
        | Invalid_roman -> "Invalid roman number"
  )
