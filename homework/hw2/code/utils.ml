open Term
open Lang

module T = Term
module L = Lang

let rec typ_to_sort : typ -> sort
= fun typ ->
  match typ with
  | Ty_Basic etyp -> etyp_to_sort etyp
  | Ty_FArr (etyp,_) -> S_Arr (S_Int, etyp_to_sort etyp)
  | Ty_DArr etyp -> S_Arr (S_Int, etyp_to_sort etyp)
  | Ty_Null -> failwith "typ_to_sort: invalid conversion"

and etyp_to_sort : etyp -> sort
= fun etyp ->
  match etyp with
  | Ty_Int -> S_Int
  | Ty_Bool -> S_Bool


(* Use an int-typed variable (with the same name)
   to represent the memory address of each array variable *)
let get_addr : L.var -> L.var
= fun v ->
  match snd v with
  | Ty_DArr _ | Ty_FArr _ -> (fst v, Ty_Basic (Ty_Int))
  | _ -> failwith "get_addr: invalid variable type"

(* 'len' in the source program.
   Modeled as an array from int (memory address) to int *)
let len_src : L.var = ("len", Ty_DArr (Ty_Int))

(* 'len' in the SMT encoding *)
let len_smt : T.var = ("len", S_Arr (S_Int, S_Int))
