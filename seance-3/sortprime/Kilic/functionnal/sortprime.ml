let rec divFilter x = function
  | [] -> []
  | h::r when h mod x = 0 -> divFilter x r
  | h::r -> h::(divFilter x r)
               
let rec sieve = function
  | [] -> failwith "error"
  | [x] -> x
  | x::r -> (sieve (divFilter x r))
            
let gen n =
  let rec gen_aux n i =
    if i = n then [n]
    else i::(gen_aux n (i+1))
  in gen_aux n 2
    
let add_prime_number n l =
  if sieve (gen n) = n then n :: l else l 

let rec primes_number l =
  try 
    let n = Scanf.scanf "%d" (fun x -> x) in
    primes_number (add_prime_number n l)
  with _ -> l

let () =
  let l = primes_number [] in
  List.sort compare l |>
  List.iter (fun i -> Printf.printf "%d\n" i)
