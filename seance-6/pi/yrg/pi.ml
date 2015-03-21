open Int64
open Functory.Cores
let () = set_number_of_cores 4

let rec count (k, n) o =
  if o = 0 then (k, n) else
    let x = Random.float 1. and y = Random.float 1. in
    let n = succ n in
    count (if x *. x +. y *. y <= 1. then (succ k, n) else (k, n)) (o - 1)

let count o =
  count (zero, zero) o

let show (k, n) =
  Printf.sprintf "(%s, %s)" (to_string k) (to_string n)

let pi (k, n) = Big_int.(
  let k = big_int_of_int64 k in
  let n = big_int_of_int64 n in
  let p = num_digits_big_int n * int_of_float (log10 (Int64.(to_float max_int))) in
  Printf.printf "Precision <= %d\n" p;
  div_big_int (
    mult_big_int k (mult_big_int (big_int_of_int 4) (power_int_positive_int 10 (1 + p)))
  ) n
)

let show_pi (k, n) =
  Big_int.string_of_big_int (pi (k, n))

let main =
  let total = int_of_string Sys.argv.(1) in
  let split k =
    let m = total / k in
    Array.(to_list (make k m))
  in
  let fold (k1, n1) (k2, n2) = (Int64.add k1 k2, Int64.add n1 n2) in
  let (k, n) = map_local_fold ~f:count ~fold (zero, zero) (split (int_of_string Sys.argv.(2))) in
  Printf.printf "%s\n" (show_pi (k, n))
