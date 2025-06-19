type t =
  | True | False
  | Not of t
  | And of t * t
  | Or of t * t
  | BinRel of brel * t * t
  | Imply of t * t
  | Iff of t * t
  | Forall of var list * t
  | Exists of var list * t
  | Sorted of t * t * t
  | Partitioned of t * t * t * t * t

  | Int of int
  | Var of var
  | Len of var (* size of array *)
  | BinOp of bop * t * t
  | Read of t * t (* arr[i] *)
  | Write of t * t * t (* A[i] := v, return the modified A *)

and brel = Eq | Neq | Leq | Lt | Geq | Gt
and bop = Plus | Minus | Mul

and var = vid * sort
and vid = string

and sort =
  | S_Int | S_Bool
  | S_Arr of sort * sort
  | S_Null

type term = t
type formula = term  (* formula is a bool-typed term *)

val to_string : t -> string
val to_string_term : t -> string (* alias for to_string *)
val to_string_formula : t -> string (* alias for to_string *)

val to_string_bop : bop -> string
val to_string_brel : brel -> string
