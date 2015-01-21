(** 
   To compile this file, use : ocamlopt -o speed unix.cmxa speed.ml 
   
    Usage : ./speed ?N.
   
    You can provide an argument at the execution to set the number of executions
    for the command test. By defaut, this number is 10. 

    Example : ./speed 20 will try to execute the test 20 times and to do the 
    average with this number.
*)

let cmd = "./anagram ironique écran aube soigneur cuvé argent Tanger chicane"

exception ExecutionError

(* [exec_times cmd n] returns a list of size [n] in which every element 
   corresponds to the time execution of the bash command [cmd]. *)
let rec exec_times : string -> int -> float list = fun cmd n ->
  let diff : string -> float = fun cmd ->
    let b = Unix.gettimeofday () in
    if Sys.command cmd != 0 then raise ExecutionError
    else Unix.gettimeofday() -. b in
  
  match n with
  | 0 -> []
  | _ -> (diff cmd) :: exec_times cmd (n-1)

(* [speed_of_anagram path cmd n] executes [n] times the command [cmd] and 
   displays his average execution time. *)
let speed_of_anagram path cmd n =
  if Sys.is_directory path then begin 
    Sys.chdir path;
    Printf.printf "Lanching \"%s\" of %s %d times : %!" cmd path n;
    if Sys.file_exists "anagram" then
      let cmd = Printf.sprintf "%s > /dev/null 2> /dev/null" cmd in
      try 
        let l = exec_times cmd n in
        let average_time = List.fold_left (fun sum x -> x +. sum) 0. l in
        Printf.printf "\027[1;32mOK\027[0m\n%!";
        Printf.printf "\027[1;32mAverage time = %.2f s\027[0m\n%!"
          (average_time /. float_of_int n)         
      with ExecutionError -> 
        Printf.printf "\027[1;31mKO\027[0m\n%!"
    else
      Printf.printf "\027[1;31mKO\027[0m\n%!";
    Sys.chdir ".."
  end

let () =  
  let n = ref 0 in 
  if Array.length Sys.argv <= 1 then n := 10
  else n := (int_of_string Sys.argv.(1));
  let allFiles = Sys.readdir "." in
  Array.iter (fun path -> speed_of_anagram path cmd !n) allFiles

  
  

