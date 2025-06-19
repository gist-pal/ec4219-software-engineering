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
type formula = term (* formula is a bool-typed term *)

let to_string_bop bop =
  match bop with
  | Plus -> "+" | Minus -> "-" | Mul -> "*"

let to_string_brel brel =
  match brel with
  | Eq -> "==" | Neq -> "!="
  | Lt -> "<"  | Leq -> "<="
  | Gt -> ">"  | Geq -> ">="

let rec to_string : t -> string
= fun term ->
  match term with
  | True -> "true"
  | False -> "false"
  | Not t -> "!(" ^ to_string t ^ ")"
  | And (t1,t2) -> "(" ^ to_string t1 ^ " /\\ " ^ to_string t2 ^ ")"
  | Or (t1,t2) -> "(" ^ to_string t1 ^ " \\/ " ^ to_string t2 ^ ")"
  | BinRel (brel,t1,t2) -> to_string t1 ^ " " ^ to_string_brel brel ^ " " ^ to_string t2
  | Imply (t1,t2) -> "(" ^ to_string t1 ^ " -> " ^ to_string t2 ^ ")"
  | Iff (t1,t2) -> "(" ^ to_string t1 ^ " <-> " ^ to_string t2 ^ ")"
  | Forall (bvars,t) ->
    "(forall [" ^ String.concat "," (List.map fst bvars) ^ "]. " ^ to_string t ^ ")"
  | Exists (bvars,t) ->
    "(exists [" ^ String.concat "," (List.map fst bvars) ^ "]. " ^ to_string t ^ ")"
  | Sorted (a,l,u) -> "sorted (" ^ to_string a ^ "," ^ to_string l ^ "," ^ to_string u ^ ")"
  | Partitioned (a,l1,u1,l2,u2) ->
    "partitioned (" ^
      to_string a ^ "," ^ to_string l1 ^ "," ^ to_string u1 ^ "," ^
      to_string l2 ^ "," ^ to_string u2 ^ ")"

  | Int n -> string_of_int n
  | Var x -> fst x
  | Len x -> "len (" ^ fst x ^ ")"
  | BinOp (bop,t1,t2) -> "(" ^ to_string t1 ^ " " ^ to_string_bop bop ^ " " ^ to_string t2 ^ ")"
  | Read (t1,t2) -> to_string t1 ^ "[" ^ to_string t2 ^ "]"
  | Write (t1,t2,t3) -> "write (" ^ to_string t1 ^ "," ^ to_string t2 ^ "," ^ to_string t3 ^ ")"

let to_string_formula = to_string
let to_string_term = to_string
