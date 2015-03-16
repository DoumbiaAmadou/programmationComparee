open Gemeinsam
open Kommunikation

let games =
  call_api "games" `Get []

let raw_doc () =
  call_api "api" `Get []

let register user password =
  call_api "register" `Post ["user", user; "password", password]

let shutdown id =
  call_api "shutdown" `Get ["id", id]
