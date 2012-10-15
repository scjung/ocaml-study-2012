let is_alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let is_digit c = '0' <= c && c <= '9'

let uppercase c = Char.chr (65 + (Char.code c - 97))

let is_palindrome s =
  let is_same c1 c2 =
    (if c1 >= 'a' then uppercase c1 else c1) =
      (if c2 >= 'a' then uppercase c2 else c2)
  in
  let rec forward f b =
    if f >= b then
      true
    else if is_digit s.[f] || is_alpha s.[f] then
      backward f (b - 1)
    else
      forward (f + 1) b

  and backward f b =
    if b <= f then
      true
    else if is_digit s.[b] || is_alpha s.[b] then
      is_same s.[f] s.[b] && forward (f + 1) b
    else
      backward f (b - 1)
  in
  forward 0 (String.length s)
