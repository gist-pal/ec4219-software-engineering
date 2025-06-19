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


val to_string_typ : typ -> string

(* 'indent' defaults to the empty string -- that is, "" *)
val to_string_cmd : ?indent:string -> cmd -> string
val to_string_exp : exp -> string
val to_string_pgm : pgm -> string
