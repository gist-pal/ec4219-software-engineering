open Vocab
open Formula

type pgm = pre * post * fid * iparam list * rparam * cmd
and pre = formula
and post = formula
and fid = string
and iparam = typ * var
and rparam = typ * var

and var = (vid * typ)
and vid = string

and typ =
  | ETyp of etyp
  | DArray of etyp
  | FArray of etyp * int
  | NullTyp

and etyp = (* elementary type *)
  | Int
  | Bool

and lv =
  | Var of var
  | Arr of var * exp

and exp =
  | Int of int
  | Len of var
  | Lv of lv
  | Plus of exp * exp
  | Minus of exp * exp
  | Mul of exp * exp
  (* always boolean expression *)
  | True
  | False
  | Eq of exp * exp
  | Neq of exp * exp
  | Leq of exp * exp
  | Lt of exp * exp
  | Geq of exp * exp
  | Gt of exp * exp
  | Not of exp
  | Or of exp * exp
  | And of exp * exp

and cmd =
  | Decl of typ * var
  | Assign of lv * exp
  | Skip
  | Seq of cmd * cmd
  | If of exp * cmd * cmd
  | While of formula * exp * cmd
  | Assert of exp
  | Return of exp

let rec to_string_typ : typ -> string
= fun t ->
  match t with
  | ETyp et -> to_string_etyp et
  | DArray et -> to_string_etyp et ^ " " ^ "[]"
  | FArray (et,n) -> to_string_etyp et ^ " " ^ "[" ^ string_of_int n ^ "]"
  | NullTyp -> "NullTyp"

and to_string_etyp : etyp -> string
= fun et ->
  match et with
  | Int -> "int"
  | Bool -> "bool"

let rec to_string_cmd ?(indent="") : cmd -> string
= fun cmd ->
  match cmd with
  | Decl (t,x) -> indent ^ to_string_typ t ^ " " ^ fst x ^ ";"
  | Assign (lv,e) -> indent ^ to_string_lv lv ^ " = " ^ to_string_exp e ^ ";"
  | Skip -> indent ^ "skip;"
  | Assert b -> indent ^ "assert" ^ "(" ^ to_string_exp b ^ ")" ^ ";"
  | Seq (c1,c2) -> to_string_cmd ~indent c1 ^ "\n" ^ to_string_cmd ~indent c2
  | If (e,c1,c2) ->
    indent ^
    "if" ^ "(" ^ to_string_exp e ^ ")" ^ "{" ^ "\n" ^
    to_string_cmd ~indent:("  " ^ indent) c1 ^ "\n" ^
    indent ^ "}" ^ "\n" ^
    indent ^
    "else" ^ "{" ^ "\n" ^
    to_string_cmd ~indent:("  " ^ indent) c2 ^ "\n" ^
    indent ^ "}"
  | While (inv,e,c) ->
    indent ^ "while" ^ " " ^ "@L" ^ "{" ^ to_string_formula inv ^ "}" ^ " " ^
    "(" ^ to_string_exp e ^ ")" ^ " " ^ "{\n" ^
    to_string_cmd ~indent:("  " ^ indent) c ^ "\n" ^
    indent ^ "}"
  | Return e ->
    indent ^ "return " ^ to_string_exp e ^ ";"

and to_string_exp : exp -> string
= fun e ->
  match e with
  | Int n -> string_of_int n
  | Len x -> "len(" ^ fst x ^ ")"
  | Lv lv -> to_string_lv lv
  | Plus (e1,e2) -> to_string_exp e1 ^ " + " ^ to_string_exp e2
  | Minus (e1,e2) -> to_string_exp e1 ^ " - " ^ to_string_exp e2
  | Mul (e1,e2) -> to_string_exp e1 ^ " * " ^ to_string_exp e2
  | True -> "true"
  | False -> "false"
  | Eq (e1,e2) ->  to_string_exp e1 ^ " == " ^ to_string_exp e2
  | Neq (e1,e2) -> to_string_exp e1 ^ " != " ^ to_string_exp e2
  | Lt (e1,e2) ->  to_string_exp e1 ^ " < "  ^ to_string_exp e2
  | Leq (e1,e2) -> to_string_exp e1 ^ " <= " ^ to_string_exp e2
  | Gt (e1,e2) ->  to_string_exp e1 ^ " > "  ^ to_string_exp e2
  | Geq (e1,e2) -> to_string_exp e1 ^ " >= " ^ to_string_exp e2
  | Not e -> "!" ^ "(" ^ to_string_exp e ^ ")"
  | Or (e1,e2) -> "(" ^ to_string_exp e1 ^ " || " ^ to_string_exp e2 ^ ")"
  | And (e1,e2) -> "(" ^ to_string_exp e1 ^ " && " ^ to_string_exp e2 ^ ")"

and to_string_lv : lv -> string
= fun lv ->
  match lv with
  | Var x -> fst x
  | Arr (x,e) -> fst x ^ "[" ^ to_string_exp e ^ "]"

let to_string_param (typ,var) = to_string_typ typ ^ " " ^ fst var

let to_string_params params =
  string_of_list ~first:"(" ~sep:", " ~last:")" to_string_param params

let to_string_pgm (pre,post,fname,iparams,rparam,cmd) =
  "@pre " ^ "{" ^ to_string_formula pre ^ "}" ^ "\n" ^
  "@post " ^ "{" ^ to_string_formula post ^ "}" ^ "\n\n" ^
  fname ^ " " ^ to_string_params iparams ^ " " ^
    "returns" ^ " " ^ to_string_params [rparam] ^ " " ^ "{\n" ^
  to_string_cmd ~indent:"  " cmd ^ "\n" ^
  "}\n"
