val create_form : string -> string list ->
  Dom_html.inputElement Js.t

val create_table : string -> Dom_html.tableElement Js.t

val add_entry_to_table : Dom_html.tableElement Js.t ->
  string -> string -> string -> unit  

