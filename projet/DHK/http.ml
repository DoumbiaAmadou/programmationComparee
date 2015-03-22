open Utils

let make_params_form data =
  List.map (fun (field,value) -> field ^ "=" ^ value) data
  |> String.concat "&"

let string_of_meth meth = match meth with
  | `GET -> "GET"
  | `POST -> "POST"
    
let request ~meth ~url ~post_data ~get_data = 
  let req = XmlHttpRequest.create () in
  let (url,data) = match meth with
    | `POST -> url, make_params_form post_data
    | `GET ->  url ^ (make_params_form get_data), "" in
  req##_open (Js.string (string_of_meth meth), Js.string url, Js.bool true);
  req##setRequestHeader(Js.string "Cookie", doc##cookie);
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


    
    
    
