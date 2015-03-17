type program = (label option * instruction) list

and instruction = 
  | Command of Data.command
  | Store of expr * identifier
  | Jump of label
  | JumpZ of identifier * label
  | Fork

and label = Label of string

and expr = 
  | Int of int 
  | Variable of identifier
  | Binop of binop * expr * expr
  | See
  | SeeAnt

and binop =
  | Add
  | Sub
  | Mul
  | Div

and identifier = Id of string

let rec string_of_program = function
  | [] -> ""
  | linstr::t -> (sof_linstr linstr) ^ (string_of_program t)

and sof_linstr = function
  | (Some(Label s),instr) -> (String.uppercase s) ^ ">" ^ (sof_instr instr) ^ ";"
  | (None, instr) -> (sof_instr instr) ^ ";"

and sof_instr = function
  | Command c -> Data.string_of_command c
  | Store(e,Id id) -> "store!" ^ id ^ "!" ^ (sof_expr e)
  | Jump(Label s) -> "jump!" ^ (String.uppercase s)
  | JumpZ(Id id,Label s) -> "jumpifz!" ^ id ^ "!" ^ (String.uppercase s)
  | Fork -> "fork"

and sof_expr = function
  | Int i -> string_of_int i
  | Variable(Id id) -> "?" ^ id
  | Binop(b,e1,e2) -> 
     "(" ^ (sof_binop b) ^ " " ^ (sof_expr e1) ^ " " ^ (sof_expr e2) ^ ")"
  | See -> "see"
  | SeeAnt -> "see_ant"

and sof_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"

let rec mk_if c t n = 
  let (end_label,n) = label n "endif" in
  (None, Store(c, Id "condition") )
  ::(None, JumpZ(Id "condition", end_label) )
  ::t
  ::n

and mk_loop c l n = 
  let (end_label,n) = label n "endloop" in
  let loop_label = create_label "loop" in
  (Some(loop_label), Store(c,Id "condition") )
  ::(None, JumpZ(Id "condition", end_label) )
  ::l
  @(None,Jump loop_label)
  ::n


and create_label =
  let i = ref 0 in
  fun s -> incr i; Label(s ^ (string_of_int !i))

and label linstr s = match linstr with
  | [] -> assert false
  | (Some l as lab,instr)::t -> (l,(lab,instr)::t)
  | (None,instr)::t -> 
     let l = create_label s in
     (l,(Some l,instr)::t)

and mk_block = function
  | [] -> []
  | instr::t -> (None,instr)::(mk_block t)
