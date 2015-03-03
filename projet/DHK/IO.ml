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
   password [p]. Raise an HttpPostError if the request fails. *)
let register_user l p =
  let url = concat_url url "register" in 
  let post_params = [("login",l); ("password",p)] in
  try 
    let s = http_post url post_params in  
    Printf.printf "%s\n" s
  with _ -> raise HttpPostError

(* [auth_user l p] make the authentication of a user having the login [l] and 
   the password [p]. *)
let auth_user l p =
  let url = concat_url url "auth" in
  let post_params = [("login",l); ("password",p)] in
  try 
    let s = http_post url post_params in
    Printf.printf "%s\n" s
  with _ -> raise HttpPostError
