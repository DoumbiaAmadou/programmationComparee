(** Module de communication avec l'API serveur. *)
open Gemeinsam

let api_version  = 0
let base_url     = Printf.sprintf "yann.regis-gianas.org/antroid/%d"
                                  api_version

(** Fonction permettant de recuperer le type d'erreur en fonction du code erreur
    fourni par un code JSON, donne en parametre. Si aucun code d'erreur n'a ete
    trouve, renvoie l'exception provoquee par l'appel de Correct(...) *)
let read_error json =
  let open Yojson.Basic.Util in
  try  Correct (json |> member "response" |> member "error_code" |> to_int |> Fehler.of_int)
  with exn -> Error exn

(** Fonction permettant de faire un appel a l'API
    - action : correspond a l'action a effectuer
    - mode   : le type de l'action (types acceptes : Get et Post)
    - args   : liste des arguments des l'action sous forme (cmd, value)
*)
let call_api action mode args =
  let handle = Curl.init () in
  let raw_response =
    let process_string queue string =
      Queue.push string queue;
      String.length string
    in
    match mode with
    | `Get ->
       let url =
         match args with
         | [] ->
            Printf.sprintf "%s/%s" base_url action
         | _  ->
            let url_args = List.map (fun (k, v) -> k ^ "=" ^ v) args
                           |> String.concat "&" in
            Printf.sprintf "%s/%s?%s" base_url action url_args
       in
       let buf = Queue.create () in
       Curl.set_url handle url;
       Curl.set_writefunction handle (process_string buf);
       Curl.perform handle;
       Queue.fold (fun acc s -> s :: acc) [] buf |> List.rev |> String.concat ""

    | `Post ->
       let url = Printf.sprintf "%s/%s" base_url action in
       let buf = Queue.create () in
       let do_it (k, v) = Curl.CURLFORM_CONTENT (k, v, Curl.DEFAULT) in
       let post_data = List.map do_it args in
       Curl.set_url handle url;
       Curl.set_writefunction handle (process_string buf);
       Curl.set_post handle true;
       Curl.set_httppost handle post_data;
       Curl.perform handle;
       Queue.fold (fun acc s -> s :: acc) [] buf |> List.rev |> String.concat ""
  in
  Curl.cleanup handle;
  try  Correct (Yojson.Basic.from_string raw_response)
  with exn -> Error exn
