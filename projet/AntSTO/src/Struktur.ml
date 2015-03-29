(** Module de représentation des commandes pour les fourmis. *)

(**
 * Une commande de fourmi.
 *
 * Ainsi, [(x, c)] représente la commande [c] donnée à la fourmi [x].
 *)
type ant_command = int * command

and command =
  | Left
  | Right
  | Forward
  | Rest
  | Attack of int
  | Hack   of instruction list

and instruction =
  | Command of command
  | Store   of string * expression
  | Jump    of string
  | JumpIfZ of string * string
  (** [(variable, label)] *)
  | Fork

and expression =
  | Int  of int
  | Var  of string
  | Prim of prim
  | Eval of expression list

and prim =
  | Add
  | Mul
  | Div
  | Sub
  | See
  | SeeAnt

let rec string_of_ant_command (id, cmd) =
  Printf.sprintf "%d:%s" id (string_of_command cmd)

and string_of_command = function
  | Left     -> "left"
  | Right    -> "right"
  | Forward  -> "forward"
  | Rest     -> "rest"
  | Attack x -> Printf.sprintf "attack@%d" x
  | Hack xs  -> Printf.sprintf "hack@[%s]" (List.map string_of_instruction xs
                                            |> String.concat ";")

and string_of_instruction = function
  | Command c      -> string_of_command c
  | Store (x, e)   -> Printf.sprintf "store!%s!%s" x (string_of_expression e)
  | Jump l         -> Printf.sprintf "jump!%s" l
  | JumpIfZ (x, l) -> Printf.sprintf "jumpifz!%s!%s" x l
  | Fork           -> "fork"

and string_of_expression = function
  | Int x       -> string_of_int x
  | Var x       -> Printf.sprintf "?%s" x
  | Prim Add    -> "add"
  | Prim Mul    -> "mul"
  | Prim Div    -> "div"
  | Prim Sub    -> "sub"
  | Prim See    -> "see"
  | Prim SeeAnt -> "see_ant"
  | Eval es     -> Printf.sprintf "(%s)" (List.map string_of_expression es
                                          |> String.concat " ")
