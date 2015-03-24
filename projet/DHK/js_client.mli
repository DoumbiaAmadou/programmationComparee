val auth : Dom_html.inputElement Js.t -> string list -> unit

val register : Dom_html.inputElement Js.t -> string list -> unit

val show_games : string -> unit

val create_game : Dom_html.inputElement Js.t -> string list -> unit

val join_game : Dom_html.inputElement Js.t -> string list -> unit
  
val logout : unit -> unit
  
val play : Dom_html.inputElement Js.t -> string list -> unit

val destroy_game : Dom_html.inputElement Js.t -> string list -> unit

val status : Dom_html.inputElement Js.t -> string list -> string -> unit


