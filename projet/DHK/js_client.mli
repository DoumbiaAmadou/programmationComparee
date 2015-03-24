(** [auth bsubmit params] do the authentification of a user. The specified 
    parameters by the user is on the list [params]. *)
val auth : Dom_html.inputElement Js.t -> string list -> unit

(** [register bsubmit params] add a new user to the server with his login and
    his password (values are in the list [params]), if the user is not already
    exists. *)
val register : Dom_html.inputElement Js.t -> string list -> unit

(** [show_games div_id] add a table with all created games informations 
    into the div having the identifier [div_id]. *)
val show_games : string -> unit

(** [create_game bsubmit params] create a new game with the users parameters 
    [params] when clicking on the submit button [submit]. *)
val create_game : Dom_html.inputElement Js.t -> string list -> unit

(** [join_game bsubmit params] join the game whose the identifier is in the 
    list [params].*)
val join_game : Dom_html.inputElement Js.t -> string list -> unit

(** [logout] logout a user.*)
val logout : unit -> unit

(** [play bsubmit params] play the specified cmds to a specify game in the user
    parameters [params]. *)
val play : Dom_html.inputElement Js.t -> string list -> unit

(** [destory_game bsubmit params] destroy a game whose the identifier is in the 
    user parameters [params]. *)
val destroy_game : Dom_html.inputElement Js.t -> string list -> unit

(** [status bsubmit params div_id] show the informations of a specify game who
    the identifier is in the user parameters [params]. *)
val status : Dom_html.inputElement Js.t -> string list -> string -> unit


