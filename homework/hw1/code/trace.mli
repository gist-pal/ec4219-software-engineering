type trace
type t = trace

(* generate error trace *)
val gen : Pgm.pgm -> int -> Z3.Model.model -> trace

val to_string : trace -> string
