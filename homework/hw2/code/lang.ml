open Term

type pgm = pre * post * fid * iparam list * rparam * cmd
and pre = formula
and post = formula
and fid = string
and iparam = var
and rparam = var

and var = vid * typ
and vid = string

and typ =
  | Ty_Basic of etyp
  | Ty_FArr of etyp * int
  | Ty_DArr of etyp
  | Ty_Null

and etyp = (* elementary type *)
  | Ty_Int
  | Ty_Bool

and lv =
  | Var of var
  | ArrAccess of var * exp

and exp =
  (* integer-typed exp *)
  | Int of int
  | Len of var
  | Lv of lv
  | BinOp of bop * exp * exp

  (* boolean-typed exp *)
  | True
  | False
  | BinRel of brel * exp * exp
  | Not of exp
  | Or of exp * exp
  | And of exp * exp

and bop = Term.bop
and brel = Term.brel

and cmd =
  | Decl of var
  | Assign of lv * exp
  | Skip
  | Seq of cmd * cmd
  | If of exp * cmd * cmd
  | While of formula * exp * cmd
  | Assert of line * exp
  | Return of exp

and line = int

let rec to_string_typ : typ -> string
= fun typ ->
  match typ with
  | Ty_Basic et -> to_string_etyp et
  | Ty_FArr (et,size) -> to_string_etyp et ^ " " ^ "[" ^ string_of_int size ^ "]"
  | Ty_DArr et -> to_string_etyp et ^ " " ^ "[]"
  | Ty_Null -> "Ty_Null"

and to_string_etyp : etyp -> string
= fun et ->
  match et with
  | Ty_Int -> "int"
  | Ty_Bool -> "bool"

let rec to_string_cmd ?(indent="") : cmd -> string
= fun cmd ->
  match cmd with
  | Decl x -> indent ^ to_string_typ (snd x) ^ " " ^ fst x ^ ";"
  | Assign (lv,e) -> indent ^ to_string_lv lv ^ " = " ^ to_string_exp e ^ ";"
  | Skip -> indent ^ "skip;"
  | Assert (_,b) -> indent ^ "assert" ^ "(" ^ to_string_exp b ^ ")" ^ ";"
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
  | BinOp (bop,e1,e2) ->
    to_string_exp e1
    ^ " " ^ Term.to_string_bop bop ^ " "
    ^ to_string_exp e2

  | True -> "true" | False -> "false"
  | BinRel (brel,e1,e2) ->
    to_string_exp e1
    ^ " " ^ Term.to_string_brel brel ^ " "
    ^ to_string_exp e2
  | Not e -> "!" ^ "(" ^ to_string_exp e ^ ")"
  | Or (e1,e2) -> "(" ^ to_string_exp e1 ^ " || " ^ to_string_exp e2 ^ ")"
  | And (e1,e2) -> "(" ^ to_string_exp e1 ^ " && " ^ to_string_exp e2 ^ ")"

and to_string_lv : lv -> string
= fun lv ->
  match lv with
  | Var x -> fst x
  | ArrAccess (x,e) -> fst x ^ "[" ^ to_string_exp e ^ "]"

let to_string_param var =
  to_string_typ (snd var) ^ " " ^ fst var

let to_string_params params =
  let lst = List.map to_string_param params in
  "(" ^ String.concat ", " lst ^ ")"

let to_string_pgm (pre,post,fname,iparams,rparam,cmd) =
  "@pre " ^ "{" ^ to_string_formula pre ^ "}" ^ "\n" ^
  "@post " ^ "{" ^ to_string_formula post ^ "}" ^ "\n\n" ^
  fname ^ " " ^ to_string_params iparams ^ " " ^
    "returns" ^ " " ^ to_string_params [rparam] ^ " " ^ "{\n" ^
  to_string_cmd ~indent:"  " cmd ^ "\n" ^
  "}\n"
