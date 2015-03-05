(* ocamlbuild -use-ocamlfind -pkg ppx_deriving.std yatze.native *)

type hand = int list [@@deriving show]

(* Pre: sorted *)
let full : hand -> bool = function
  | [x1; x2; x3; x4; x5] ->
     (x1 = x2 && x2 = x3 && x4 = x5) || (x1 = x2 && x3 = x4 && x4 = x5)
  | _ -> assert false

(* Pre: sorted *)
let suite : hand -> bool = function
  | [x1; x2; x3; x4; x5] ->
     x5 = x4 + 1 && x4 = x3 + 1 && x3 = x2 + 1 && x2 = x1 + 1
  | _ -> assert false

let rec permutations lst =
  let rec insert x l = match l with
    | [] -> [[ x ]]
    | a :: m -> (x :: l) :: (List.map (fun y -> a :: y) (insert x m))
  in
  match lst with
  | a :: m -> List.flatten (List.map (insert a) (permutations m))
  | _ -> [lst]

let _ =

  let c =
    let d = [ 1; 2; 3; 4; 5 ; 6] in
    let add l =
      List.map (fun l -> List.map (fun x ->  x :: l) d) l
      |> List.flatten in

    [[];[];[];[];[]] |> add |> add |> add |> add |> add
    |> List.map (List.sort compare)
    |> List.sort_uniq compare in

  let foo fn = List.filter fn c
               |> List.map permutations
               |> List.flatten
               |> List.sort_uniq compare in

  let c_full = foo full in

  let c_suite = foo suite in

  print_endline "Suite:" ;
  List.iter (fun x -> print_endline (show_hand x)) c_suite ;
  print_endline "Full:" ;
  List.iter (fun x -> print_endline (show_hand x)) c_full ;
