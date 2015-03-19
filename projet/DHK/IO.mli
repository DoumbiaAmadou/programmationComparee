type cookie
  
type game_identifier


val string_of_cookie : cookie -> string

val string_of_gameid : game_identifier -> string
  
val gameid_of_string : string -> game_identifier 

(** [register_user l p] add a new user with the login [l] and the
    password [p]. *)
val register_user : string -> string -> bool

(** [auth_user l p] make the authentification of a user having the login [l]
    and the password [p]. If the request is successful, the function returns
    the cookie send by the server or raise an exception.*)
val auth_user : string -> string -> cookie

(** [create_new_game users teaser pace nb_turn nb_ant_per_player nb_player
    minimal_nb_player initial_energy initial_acid cookie] creates a new game
    and his result identifier. *)
val create_new_game :
  users:string ->
  teaser:string ->
  pace:int ->
  nb_turn:int ->
  nb_ant_per_player:int ->
  nb_player:int ->
  minimal_nb_player:int ->
  initial_energy:int -> initial_acid:int -> cookie:cookie -> game_identifier

type game_description
  
(* Pour l'instant, erreur lors de la récupération des champs *)
val show_games : cookie -> game_description list 

(** [join_game cookie id] return true if a user join the game having the
    identifier [id].*)
val join_game : cookie -> game_identifier -> bool

(** [logout cookie] logout a user. *)
val logout : cookie -> bool

(** [play cookie id cdms] send the commands [cmds] to the game having the
    identifier [id]. *)
val play : cookie -> game_identifier -> string -> (int * Data.ant) list 

(* Todo : possibilité de créer une structure *)
val game_status : cookie -> game_identifier -> unit 

(** [destroy_game cookie id] return true if the game having the identifier [id]
    is destroyed. *)
val destroy_game : cookie -> game_identifier -> bool
  

