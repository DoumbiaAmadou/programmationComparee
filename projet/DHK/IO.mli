(** [is_success response] check if the field status in [response] has for value
    completed, indicating a success. *)
val is_success : string -> bool

(** [all_games_informations] returns the list of all created games on the 
    server with the game identifier, the game creator and the teaser. *)
val all_games_informations : string -> (string * string * string) list 

(** [game_status response] return the list with all game informations contained 
    in the json string [response]. *)
val game_status : string -> (string * string) list 
