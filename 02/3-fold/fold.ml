let fold init s next e f =
  let rec aux x n =
    if n > e then x
    else aux (f x n) (next n)
  in
  aux init s

let int_fold init s e f = fold init s succ e f

(* 주의. 분모 전체를 계산하고 분자 전체를 계산한 후 두 값을 나누면 [n] 인자가
   조금만 커도 값넘침이 발생하여 파이값을 제대로 계산해 낼 수 없다. 따라서
   모두 곱한 후 나누지 말고, 항을 하나씩 구한 후 곱해 나가야 한다. *)
let eval_pi n =
  let nth n =
    (float_of_int (if n mod 2 = 0 then n + 2 else n + 1)) /.
      (float_of_int (if n mod 2 = 0 then n + 1 else n + 2))
  in
  fold 4.0 1 succ n (fun x n -> x *. nth n)
