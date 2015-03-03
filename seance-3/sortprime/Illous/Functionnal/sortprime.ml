let rec divFilter x = function
  | [] -> []
  | h::r when h mod x = 0 ->
    divFilter x r
  | h::r -> h::(divFilter x r);;


let rec sieve = function
  | [] -> failwith "error"
  | [x] -> x
  | x::r -> (sieve (divFilter x r));;

let gen n =
  let rec gen_aux n i =
    if i = n then [n]
    else i::(gen_aux n (i+1))
  in gen_aux n 2;;

let is_prime n = sieve (gen n) = n;;

let rec main () =
  try
    let n = Scanf.scanf "%d\n" (fun x -> x)
    in
    begin
      match is_prime n with
      | true -> Printf.printf "%d : premier\n" n
      | false -> Printf.printf "%d : non premier \n" n
    end;
    main ()
  with _ -> ()
;;

main ();;
