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

let rec to_string : t -> string
= fun f ->
  match f with
  | True -> "true"
  | False -> "false"
  | Var x -> x
  | Not f -> "!(" ^ to_string f ^ ")"
  | And (f1,f2) -> "(" ^ to_string f1 ^ " /\\ " ^ to_string f2 ^ ")"
  | Or (f1,f2) -> "(" ^ to_string f1 ^ " \\/ " ^ to_string f2 ^ ")"
  | Imply (f1,f2) -> "(" ^ to_string f1 ^ " -> " ^ to_string f2 ^ ")"
  | Iff (f1,f2) -> "(" ^ to_string f1 ^ " <-> " ^ to_string f2 ^ ")"
