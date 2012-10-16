type p = { x : float; y : float }

let rotate rad p =
  let sin_rad = sin rad in
  let cos_rad = cos rad in
  let x = (p.x *. cos_rad) +. (p.y *. sin_rad) in
  let y = (p.y *. cos_rad) -. (p.x *. sin_rad) in
  { x; y }

let rotate_triangle rad (p1, p2, p3) =
  (rotate rad p1, rotate rad p2, rotate rad p3)
