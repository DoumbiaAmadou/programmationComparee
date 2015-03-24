val doc : Dom_html.document Js.t

val win : Dom_html.window Js.t
    
(** [get_element_by_id id] return the element having the identifier [id]. *)
val get_element_by_id : string -> Dom_html.element Js.t

(** [input_value id] return the value of the input element having the 
    identifier [id]. *)
val input_value : string -> string

(** [clear_div div_id] remove all childs of the div having the id [div_id]. *)
val clear_div : string -> unit


