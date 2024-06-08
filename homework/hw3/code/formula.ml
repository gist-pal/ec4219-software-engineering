open Vocab

type formula =
  | True | False
  | PVar of vid
  | Sorted of term * term * term
  | Partitioned of term * term * term * term * term
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | BinRel of brel * term * term
  | Imply of formula * formula
  | Iff of formula * formula
  | Forall of var list * formula
  | Exists of var list * formula
  | Hole of hid
and hid = int

and term =
  | Int of int
  | Var of var
  | Len of var (* len of array *)
  | Read of term * term (* A[i] *)
  | Write of term * term * term (* A[i] := v, return the modified A *)
  | BinOp of bop * term * term * sort

and var = (vid * sort)
and vid = string

and brel = Geq | Gt | Leq | Lt | Eq | Neq
and bop = Add | Sub | Mul

and sort =
  | ESort of esort
  | Array of sort * sort
  | NullSort

and esort =
  | Int
  | Bool

let rec to_string_formula : formula -> string
= fun f ->
  match f with
  | True -> "true"
  | False -> "false"
  | Not f -> "!(" ^ to_string_formula f ^ ")"
  | And (f1,f2) -> "(" ^ to_string_formula f1 ^ " /\\ " ^ to_string_formula f2 ^ ")"
  | Or (f1,f2) -> "(" ^ to_string_formula f1 ^ " \\/ " ^ to_string_formula f2
  | PVar p -> p
  | Sorted (a,l,u) -> "sorted (" ^ to_string_term a ^ "," ^ to_string_term l ^ "," ^ to_string_term u ^ ")"
  | Partitioned (a,l1,u1,l2,u2) ->
    "partitioned (" ^
      to_string_term a ^ "," ^ to_string_term l1 ^ "," ^ to_string_term u1 ^ "," ^
      to_string_term l2 ^ "," ^ to_string_term u2 ^ ")"
  | BinRel (brel,t1,t2) ->
    (match brel with
     | Geq -> to_string_term t1 ^ " >= "  ^ to_string_term t2
     | Gt ->  to_string_term t1 ^ " > "   ^ to_string_term t2
     | Leq -> to_string_term t1 ^ " <= "  ^ to_string_term t2
     | Lt ->  to_string_term t1 ^ " < "   ^ to_string_term t2
     | Eq ->  to_string_term t1 ^ " = "   ^ to_string_term t2
     | Neq -> to_string_term t1 ^ " != "  ^ to_string_term t2)
  | Imply (f1,f2) -> "(" ^ to_string_formula f1 ^ " -> " ^ to_string_formula f2 ^ ")"
  | Iff (f1,f2) -> "(" ^ to_string_formula f1 ^ " <-> " ^ to_string_formula f2 ^ ")"
  | Forall (bvars,f) ->
    "(" ^ "forall " ^ (string_of_list ~first:"[" ~sep:"," ~last:"]" fst bvars)
    ^ ". " ^ to_string_formula f ^ ")"
  | Exists (bvars,f) ->
    "(" ^ "exists " ^ (string_of_list ~first:"[" ~sep:"," ~last:"]" fst bvars)
    ^ ". " ^ to_string_formula f ^ ")"
  | Hole hid -> "H#" ^ string_of_int hid

and to_string_term : term -> string
= fun term ->
  match term with
  | Int n -> string_of_int n
  | Var x -> fst x
  | Len x -> "len" ^ "(" ^ fst x ^ ")"
  | Read (t1,t2) -> to_string_term t1 ^ "[" ^ to_string_term t2 ^ "]"
  | Write (t1,t2,t3) -> "write(" ^ to_string_term t1 ^ "," ^ to_string_term t2 ^ "," ^ to_string_term t3 ^ ")"
  | BinOp (bop,t1,t2,_) ->
    (match bop with
     | Add -> "(" ^ to_string_term t1 ^ " + " ^ to_string_term t2 ^ ")"
     | Sub -> "(" ^ to_string_term t1 ^ " - " ^ to_string_term t2 ^ ")"
     | Mul -> "(" ^ to_string_term t1 ^ " * " ^ to_string_term t2 ^ ")")
