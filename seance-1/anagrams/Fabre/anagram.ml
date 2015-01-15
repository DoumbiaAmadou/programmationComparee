module Mot = struct
                type t = string
                let compare = String.compare
             end
module DictMot = Map.Make(Mot)

let content_of_file f = let ic = open_in f in
                                let n = in_channel_length ic in
                                let s = String.create n in
                                really_input ic s 0 n;
                                close_in ic; s

let cut s = Str.split (Str.regexp s)

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let implode l =
    let res = String.create (List.length l) in
    let rec imp i = function
        | [] -> res
        | c :: l -> res.[i] <- c; imp (i + 1) l in
    imp 0 l

let char_to_str = implode
let str_to_char = explode

let gen_key w = let letters = str_to_char w in
                let letters = List.sort Pervasives.compare letters in
                char_to_str letters

let add d w = let k = gen_key w in 
              let ws = try DictMot.find k d
                       with Not_found -> [] in 
              DictMot.add k (w::ws) d

let find d w = let k = gen_key w in DictMot.find k d

let m' = let fn = "../words" in
         let s = content_of_file fn in
         let mots = cut "\n" s in
         List.fold_left add DictMot.empty mots

let liste_args = let liste_args = Array.sub 
                    (Sys.argv) 1 (Array.length Sys.argv - 1) in
                 Array.to_list liste_args

let () = let liste w = try find m' w 
                       with Not_found -> [w] in
         let liste_s w = String.concat " " (liste w) in
         List.iter 
            (fun w -> Printf.printf "%s: %s\n" w (liste_s w)) 
            (liste_args)
