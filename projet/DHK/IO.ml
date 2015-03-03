open Nethttp_client.Convenience

let prefix_url = "http://yann.regis-gianas.org/antroid/"

let version = "0"

let make_complete_url purl v = purl ^ v

let register l p =
  let url = make_complete_url prefix_url version in
  let url = Printf.sprintf "%s/%s" url "register" in 
  let post_params = [("login",l); ("password",p)] in 
  let s = http_post url post_params in
  Printf.printf "%s\n" s
