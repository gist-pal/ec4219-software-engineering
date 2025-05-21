(* create a variable X_t for representing a shared variable X at time t *)
val v2 : Formula.id * int -> Formula.formula

(* create a variable H_t for representing a thread H is in state l at time t *)
val v3 : Pgm.tid * int * int -> Formula.formula

(* create a variable A_lt for representing a thread A is in state l at time t *)
val a : int * int -> Formula.formula

(* create a variable B_lt for representing a thread A is in state l at time t *)
val b : int * int -> Formula.formula
