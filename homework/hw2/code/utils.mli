open Term
open Lang

module T = Term
module L = Lang

val typ_to_sort : typ -> sort

(* Use an int-typed variable (with the same name)
   to represent the memory address of each array variable *)
val get_addr : L.var -> L.var

(* 'len' in the source program.
   Modeled as an array from int (memory address) to int *)
val len_src : L.var

(* 'len' in the SMT encoding *)
val len_smt : T.var
