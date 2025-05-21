open Formula

type status = SAT | UNSAT | UNK

val check_sat : formula -> (status * Z3.Model.model option)
val string_of_model : Z3.Model.model -> string

(* return true if the formula is true under the given model *)
val is_true: Z3.Model.model -> formula -> bool
