type t = thread * thread

and pgm = t
and thread = tid * lcmd list
and lcmd = label * cmd
and label = int
and tid = string
and var = string

and cmd =
  | MayGoto of label
  | If of var * label * label
  | Set of var * bool * label
  | Critical of label

val get_tid : thread -> tid
val get_lcmds : thread -> lcmd list
val get_last_state : thread -> label

(* collect all shared variables in a program *)
val collect_vars : pgm -> var BatSet.t
