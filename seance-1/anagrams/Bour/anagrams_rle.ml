(*** Routines *)

(** Iterate on all lines of a file *)
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

(** Compute anagram equivalence class of a string *)

(* This value never escapes, however memory pressure is the main bottleneck
     when computing batch of classes.
     Allocating the array once is safe and efficient. *)
let arr = Array.make 256 0

let anagram_class s =

  (* Step 1. Compute distribution of input characters *)
  let cmin = ref max_int in
  let cmax = ref min_int in
  (* Count character occurences *)
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    arr.(c) <- succ arr.(c);
    if c < !cmin then cmin := c;
    if c > !cmax then cmax := c;
  done;
  let cmin = !cmin and cmax = !cmax in

  (* Step 2. RLE compress result
     Otherwise memory is a problem with batch of short words *)

  (* Step 2a. Compute number of RLE chunks *)
  let chunks = ref 0 in
  (* Number of 0s in a stride *)
  let contiguous = ref cmin in
  (* Compute result length *)
  for i = cmin to cmax do
    if arr.(i) = 0 then
      incr contiguous
    else
      begin
        incr chunks;
        contiguous := 0;
      end
  done;
  let chunks = !chunks in

  (* Step 2b. Fill result *)
  let result = String.make (chunks * 2) '\000' in
  let chunks = ref 0 in
  let contiguous = ref cmin in
  for i = cmin to cmax do
    if arr.(i) = 0 then
      incr contiguous
    else
      begin
        result.[!chunks * 2] <- Char.unsafe_chr !contiguous;
        result.[!chunks * 2 + 1] <- Char.unsafe_chr arr.(i);
        incr chunks;
        contiguous := 0;
      end;
    arr.(i) <- 0
  done;
  result

(* Specialize hash table, to avoid polymorphic operators *)
module H = Hashtbl.Make(struct
    type t = string
    let equal (a : t) b =
      a = b
    let hash (a : t) = Hashtbl.hash a
  end)

(* From a list of values, partition them into an hashtable indexed
   by an arbitrary equivalence_class *)
let compute_equivalence_table equivalence_class iter_keys iter_values =
  let table = H.create 257 in
  iter_keys (fun key ->
      let cls = equivalence_class key in
      try
        let keys, values = H.find table cls in
        keys := key :: !keys
      with Not_found ->
        H.add table cls (ref [key], ref []));
  iter_values (fun value ->
      let cls = equivalence_class value in
      try
        let keys, values = H.find table cls in
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
  H.iter (fun _ (keys, values) ->
      let result = ": " ^ String.concat " " !values in
      List.iter (fun arg ->
          print_string arg;
          print_endline result)
        !keys
    )
    table

let () = main ()
