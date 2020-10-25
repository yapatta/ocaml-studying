(* 原始帰納 *)
let rec add (x, y) = if y = 0 then x else add (x, y - 1) + 1

let rec mul (x, y) = if y = 0 then 0 else add (x, mul (x, y - 1))

let rec exp (x, y) = if y = 0 then 1 else mul (x, exp (x, y - 1))

let rec fact x = if x = 0 then 1 else mul (x, fact (x - 1))

let rec pre y = if y = 0 then 0 else y - 1

let rec sub (x, y) = if y = 0 then x else pre (sub (x, y - 1))

let rec max (x, y) =
  if y = 0 then x else if x = 0 then y else max (x - 1, y - 1) + 1

let rec min (x, y) =
  if x = 0 then x else if y = 0 then y else min (x - 1, y - 1) + 1

(* 原始帰納関数でない *)
let rec ack (x, y) =
  if x = 0 then y + 1
  else if y = 0 then ack (x - 1, 1)
  else ack (x - 1, ack (x, y - 1))
