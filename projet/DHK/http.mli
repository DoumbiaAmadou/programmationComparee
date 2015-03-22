val http_post : url:string ->
  post_params:(string * string) list ->
  XmlHttpRequest.xmlHttpRequest Js.t
    
val http_get : url:string ->
  get_params:(string * string) list ->
  XmlHttpRequest.xmlHttpRequest Js.t
