open Nethttp_client.Convenience

exception HttpPostError
exception HttpGetError
  
(* The static prefix of the server URL *)
let prefix_url = "http://yann.regis-gianas.org/antroid/"

(* The current version of the API *)
let version = "0"

(* The first part url of the server *)
let url = prefix_url ^ version

(* [concat_url p s] return the concatenation between [p] and [s]. *)
let concat_url p s = Printf.sprintf "%s/%s" p s

(* [register_user l p] try to add a new user with the login [l] and the 
   password [p]. Raise an HttpPostError exception if the request fails. *)
let register_user l p =
  let url = concat_url url "register" in 
  let post_params = [("login",l); ("password",p)] in
  try 
    let s = http_post url post_params in  
    Printf.printf "%s\n" s
  with _ -> raise HttpPostError

(* [auth_user l p] make the authentication of a user having the login [l] and 
   the password [p]. Raise an HttpPostError exception if the request fails.*)
let auth_user l p =
  let url = concat_url url "auth" in
  let post_params = [("login",l); ("password",p)] in
  try 
    let s = http_post url post_params in
    Printf.printf "%s\n" s
  with _ -> raise HttpPostError

(* [create_new_game users teaser pace nb_turn nb_ant_per_player nb_player
   minimal_nb_player initial_energy initial_acid] try to create a new game.
   Raise an HttpGetError exception if the request fails. *)
let create_new_game ~users ~teaser ~pace ~nb_turn ~nb_ant_per_player
    ~nb_player ~minimal_nb_player ~initial_energy ~initial_acid =  
  let url = concat_url url "create" in
  let url = url ^ "?users=" ^ users ^
            "&teaser=" ^ teaser ^
            "&pace=" ^ (string_of_int pace) ^
            "&nb_turn=" ^ (string_of_int nb_turn) ^
            "&nb_ant_per_player=" ^ (string_of_int nb_ant_per_player) ^
            "&nb_player=" ^ (string_of_int nb_player) ^
            "&minimal_nb_player=" ^ (string_of_int minimal_nb_player) ^
            "&initial_energy=" ^ (string_of_int initial_energy) ^
            "&initial_acid=" ^ (string_of_int initial_acid) in
  try
    let s = http_get url in
    Printf.printf "%s\n" s
  with _ -> raise HttpGetError 

let destroy_game id =
  let url = concat_url url "destroy" in
  let url = url ^ "?id=" ^ id  in
  try
    let s = http_get url in
    Printf.printf "%s\n" s
  with _ -> raise HttpGetError

  
