open Gemeinsam

let print_doc = ref false

let args =
  let open Arg in
  [
    "-print_doc", Set print_doc, "Print the raw JSON api, then exit.";
  ]

let _ =
  Arg.parse args failwith "";
  if !print_doc then
    match Programmierschnittstelle.api () with
    | Correct json ->
       Yojson.Basic.pretty_to_string json |> print_endline
    | Error exn ->
       print_endline @$ Printexc.to_string exn
