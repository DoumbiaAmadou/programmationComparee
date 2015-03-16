open Gemeinsam

exception FieldNotFound of string
exception IllFormedJson

let api_version  = 0
let base_url     = Printf.sprintf "yann.regis-gianas.org/antroid/%d"
                                  api_version
let api_url      = Printf.sprintf "%s/api" base_url
let auth_url     = Printf.sprintf "%s/auth" base_url
let create_url   = Printf.sprintf "%s/create" base_url
let destroy_url  = Printf.sprintf "%s/destroy" base_url
let games_url    = Printf.sprintf "%s/games" base_url

let is_error json =
  let open Yojson.Basic.Util in
  json |> member "status" |> to_string = "error"

let is_success json =
  let open Yojson.Basic.Util in
  json |> member "status" |> to_string = "completed"

let read_error json =
  let open Yojson.Basic.Util in
  try  Correct (json |> member "response" |> member "error_code" |> to_int |> Fehler.of_int)
  with exn -> Error exn

let process_string queue string =
  Queue.push string queue;
  String.length string

let call_api url mode args =
  let handle = Curl.init () in
  let raw_response =
    match mode with
    | `Get ->
       let url =
         match args with
         | [] -> url
         | _  ->
            let url_args = List.map (fun (k, v) -> k ^ "=" ^ v) args
                           |> String.concat "&" in
            Printf.sprintf "%s?%s" url url_args
       in
       let buf = Queue.create () in
       Curl.setopt handle (Curl.CURLOPT_URL url);
       Curl.setopt handle (Curl.CURLOPT_WRITEFUNCTION (process_string buf));
       Curl.perform handle;
       Queue.fold (fun acc s -> s :: acc) [] buf |> List.rev |> String.concat ""

    | `Set -> ""
    ;
  in
  Curl.cleanup handle;
  try  Correct (Yojson.Basic.from_string raw_response)
  with exn -> Error exn

let get_doc () = call_api api_url `Get []
