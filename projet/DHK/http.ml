open Utils

(** [concat_parameters data] return the concatenation between every elements in 
   [data] using the character '&' as separator. *)
let concat_parameters data =
  List.map (fun (field,value) -> field ^ "=" ^ value) data
  |> String.concat "&"
    
(** [string_of_meth m] return the string corresponding to the method [m]. *)
let string_of_meth m = match m with
  | `GET -> "GET"
  | `POST -> "POST"

(**  [request method url post_data get_data] do a http request using the method
     [meth] to the target [url]. The parameter [post_data] is using in the case 
     of a POST request and the parameter [get_data] is using in the case of a 
     GET request. *)
let request ~meth ~url ~post_data ~get_data = 
  let req = XmlHttpRequest.create () in
  let (url,data) = match meth with
    | `POST -> url, concat_parameters post_data
    | `GET ->  url ^ (concat_parameters get_data), "" in
  req##_open (Js.string (string_of_meth meth), Js.string url, Js.bool true);
  req##setRequestHeader
    (Js.string "Access-Control-Allow-Credentials", Js.string "true");
  req##setRequestHeader(Js.string "Access-Control-Allow-Origin", Js.string url);
  req##setRequestHeader(Js.string "setDisableHeaderCheck", Js.string "true");
  req##setRequestHeader(Js.string "cookie", doc##cookie);
  req##setRequestHeader(Js.string "withCredentials", Js.string "true");
  req##setRequestHeader(Js.string "Content-Type",
        		Js.string "application/x-www-form-urlencoded");
  Printf.printf "url = %s\n" url;
  Printf.printf "data = %s\n" data;
  req##send (Js.some (Js.string data));
  req 

let http_post ~url ~post_params = 
  request ~meth:`POST
    ~url:url
    ~post_data:post_params
    ~get_data:[]

let http_get ~url ~get_params =
  request
    ~meth:`GET
    ~url:url
    ~post_data:[]
    ~get_data:get_params


    
    
    
