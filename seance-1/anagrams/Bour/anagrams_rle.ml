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

let arr = Array.make 256 0

let eq_class s =
  let count = ref 0 in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    let k = arr.(c) + 1 in
    if k = 1 then incr count;
    arr.(c) <- k
  done;
  (* RLE *)
  let chunks = ref 0 in
  let contiguous = ref 0 in
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
        incr chunks;
        result.(!chunks) <- arr.(i);
        incr chunks;
        contiguous := 0;
      end;
    arr.(i) <- 0
  done;
  result

let words_file = "../words"

let main () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv -1) in
  let table = Hashtbl.create (Array.length args) in
  Array.iter (fun arg ->
      let cls = eq_class arg in
      try
        let args, matches = Hashtbl.find table cls in
        args := arg :: !args
      with Not_found ->
        Hashtbl.add table cls (ref [arg], ref []))
    args;
  iter_line (fun word ->
      try
        let _, matches = Hashtbl.find table (eq_class word) in
        matches := word :: !matches
      with Not_found -> ()
    ) words_file;
  Hashtbl.iter (fun _ (args, matches) ->
      let result = ": " ^ String.concat " " !matches in
      List.iter (fun arg -> print_string arg; print_endline result)
        !args
    )
    table

let () = main ()
