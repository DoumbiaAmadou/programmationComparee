(* Routines *)

(* Iterate on all lines of a file *)
let iter_line f ic =
  try
    while true do
      f (input_line ic);
    done
  with End_of_file -> ()

let iter_line f file =
  let ic = open_in file in
  try
    iter_line f ic;
    close_in ic;
  with exn ->
    close_in_noerr ic;
    raise exn

(* Compute anagram equivalence class of a string *)
let anagram_class =
  (* This value never escapes, however memory pressure is the main bottleneck
     when computing batch of classes.
     Allocating the array once is safe and efficient. *)
  let arr = Array.make 256 0 in
  fun s ->
    (* Count character occurences *)
    for i = 0 to String.length s - 1 do
      let c = Char.code s.[i] in
      arr.(c) <- arr.(c) + 1
    done;
    (* RLE compress result
       Otherwise memory is a problem batch of short words *)
    (* Number of RLE chunks *)
    let chunks = ref 0 in
    (* Number of 0 in a stride *)
    let contiguous = ref 0 in
    (* Compute result length *)
    for i = 0 to 255 do
      if arr.(i) = 0 then
        incr contiguous
      else if !contiguous = 0 then
        incr chunks
      else
        begin
          chunks := !chunks + 2;
          contiguous := 0;
        end
    done;
    (* Fill result *)
    let result = Array.create !chunks 0 in
    let chunks = ref 0 in
    let contiguous = ref 0 in
    for i = 0 to 255 do
      if arr.(i) = 0 then
        incr contiguous
      else if !contiguous = 0 then
        begin
          result.(!chunks) <- arr.(i);
          incr chunks
        end
      else
        begin
          result.(!chunks) <- -(!contiguous);
          result.(!chunks + 1) <- arr.(i);
          chunks := !chunks + 2;
          contiguous := 0;
        end;
      arr.(i) <- 0
    done;
    result

(* From a list of values, partition them into an hashtable indexed
   by an arbitrary equivalence_class *)
let compute_equivalence_table equivalence_class iter_keys iter_values =
  let table = Hashtbl.create 257 in
  iter_keys (fun key ->
      let cls = equivalence_class key in
      try
        let keys, values = Hashtbl.find table cls in
        keys := key :: !keys
      with Not_found ->
        Hashtbl.add table cls (ref [key], ref []));
  iter_values (fun value ->
      let cls = equivalence_class value in
      try
        let keys, values = Hashtbl.find table cls in
        values := value:: !values
      with Not_found -> ());
  table

(* Command *)

let words_file = "../words"

let main () =
  let keys f =
    for i = 1 to Array.length Sys.argv - 1 do
      f Sys.argv.(i)
    done
  and values f = iter_line f words_file
  in
  let table =
    compute_equivalence_table anagram_class keys values
  in
  Hashtbl.iter (fun _ (keys, values) ->
      let result = ": " ^ String.concat " " !values in
      List.iter (fun arg ->
          print_string arg;
          print_endline result)
        !keys
    )
    table

let () = main ()
