(**  [http_post url post_params] send a http request to the target [url] using
 the method POST with the post arguments  [post_params]. *)
val http_post : url:string ->
  post_params:(string * string) list ->
  XmlHttpRequest.xmlHttpRequest Js.t

(**  [http_get url get_params] send a http request to the target [url] using
 the method GET with the get arguments [get_params]. *)
val http_get : url:string ->
  get_params:(string * string) list ->
  XmlHttpRequest.xmlHttpRequest Js.t
