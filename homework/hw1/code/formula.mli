type t =
  | True | False
  | Var of id
  | Not of t 
  | And of t * t
  | Or of t * t
  | Imply of t * t
  | Iff of t * t

and formula = t
and id = string

val to_string : t -> string
