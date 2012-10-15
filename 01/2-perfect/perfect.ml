let is_perfect n =
  let rec sum_of_divisors sum divisor =
    if divisor >= n then
      sum
    else if n mod divisor = 0 then
      sum_of_divisors (sum + divisor) (divisor + 1)
    else
      sum_of_divisors sum (divisor + 1)
  in
  sum_of_divisors 0 1 = n

(* [1~n] 사이의 홀수 중에 완전수가 있는지 검사한다.  *)
let no_odd_perfect_number n =
  let rec aux n =
    if n < 1 then
      true
    else if is_perfect n then
      false
    else
      aux (n - 2)
  in
  aux (if n mod 2 = 0 then n - 1 else n)
