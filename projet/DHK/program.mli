type program = (label option * instruction) list
and instruction =
    Command of Data.command
  | Store of expr * identifier
  | Jump of label
  | JumpZ of identifier * label
  | Fork
and label = Label of string
and expr =
    Int of int
  | Variable of identifier
  | Binop of binop * expr * expr
  | See
  | SeeAnt
and binop = Add | Sub | Mul | Div
and identifier = Id of string

val mk_if :
  expr ->
  (label option * instruction) list ->
  (label option * instruction) list -> (label option * instruction) list

