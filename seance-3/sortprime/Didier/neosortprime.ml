open List

let rec is_known_prime n = function
  | [] -> false
  | x::t -> if x < n
    then false
    else if x = n
    then true
    else is_known_prime n t

and add_primes_in inter primes =
  let rec make (n,p) =
    if p=n 
    then [p]
    else p::(make (n,p+1))
  and add_if_prime primes n =
    if for_all (fun x -> n mod x <> 0) primes
    then n::primes
    else primes
  in
  fold_left add_if_prime primes (make inter)

and is_prime n primes max =  
  let primes = 
    if n > max 
    then add_primes_in (n,max) primes
    else primes
  in (is_known_prime n primes,primes)
;;

let rec loop primes maxn = 
  try
    let n = Scanf.scanf "%d\n" (fun x -> x) 
    in
    match is_prime n primes maxn with
    | (b,primes) -> 
      if b 
      then Printf.printf "%d : Prime\n" n
      else Printf.printf "%d : Not prime\n" n;
      loop primes (max n maxn)
  with _ -> primes 
;;

let () = ignore(loop [2] 2)

