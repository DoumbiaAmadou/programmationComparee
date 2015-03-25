(** [create_form div_id labels ] add every label from [labels] with a input 
    text in the div having the identifier [id]. *)
val create_form : string -> string list -> Dom_html.inputElement Js.t

(** [create_table id] return a table element having the identifier [id]. *)
val create_table : string -> Dom_html.tableElement Js.t

(** [add_entry_to_table table id creator teaser] add a new line into the 
    the table having the identifier [id] in which there is the identifier, 
    the creator and the teaser of a game. *)
    
val add_entry_to_table : Dom_html.tableElement Js.t ->
  string -> string -> string -> unit

(** [show_game_status div_id values] add a new table element into the div 
    [div_id] with all informations from [values] of a specify game. *)
val show_game_status : string -> (string * string) list -> unit


