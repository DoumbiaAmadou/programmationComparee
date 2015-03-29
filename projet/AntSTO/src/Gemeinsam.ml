(** Module contenant le code partagé. *)

(** Un type permettant d'encoder des erreurs. *)
type ('a, 'b) either =
  | Correct of 'a
  | Error   of 'b

(** Un opérateur permettant de se passer de parenthèses superflues. *)
let ( @$ ) f x = f x
