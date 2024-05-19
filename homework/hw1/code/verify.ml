open Vocab
open Lang
open Formula
open Solver

module F = Formula
module L = Lang

(* return true when exp is a bool-typed expression *)
let rec is_bexp : L.exp -> bool
= fun exp ->
  match exp with
  | L.Int _ | L.Len _
  | L.Plus _ | L.Minus _ | L.Mul _ -> false
  | L.Lv (Var v) -> snd v = ETyp Bool
  | L.Lv (L.Arr (v,_)) ->
    (match snd v with
     | DArray Bool | FArray (Bool, _) -> true
     | _ -> false)
  | _ -> true

(* convert types in language into types in formula *)
let rec typ_to_sort : typ -> sort
= fun typ ->
  match typ with
  | ETyp et -> ESort (etyp_to_esort et)
  | DArray et -> Array (ESort Int, ESort (etyp_to_esort et))
  | FArray (et,_) -> Array (ESort Int, ESort (etyp_to_esort et))
  | NullTyp -> failwith "verify.ml : NullTyp"

and etyp_to_esort : etyp -> esort
= fun etyp ->
  match etyp with
  | L.Int -> F.Int
  | L.Bool -> F.Bool

(* the built-in function 'len' can be modelled as an array *)
let len_arr = ("len", F.Array (ESort Int, ESort Int))

(* implicit assumption (function precondition): forall(a). len(a) >= 0 *)
let len_non_neg =
  let a = ("@a", ESort Int) in
  F.Forall ([a], F.BinRel (F.Geq, F.Read (F.Var len_arr, F.Var a), F.Int 0))

(* model the memory address of each array var as integer var (with the same name) *)
(* you may need this function when generating preconditions for assignments *)
let get_addr : L.var -> L.var
= fun v ->
  match snd v with
  | DArray _ | FArray _ -> (fst v, ETyp Int)
  | _ -> assert false

(* convert boolean expressions of our language into formulas *)
let rec trans_b : L.exp -> F.formula
= fun exp -> raise NotImplemented (* TODO *)

(* convert (non-boolean) expressions into terms *)
and trans_e : L.exp -> F.term
= fun exp ->
  match exp with
  | L.Int n -> F.Int n
  | L.Len v -> F.Read (F.Var len_arr, F.Var (fst v, ESort Int))
  | L.Lv (L.Var v) -> F.Var (fst v, typ_to_sort(snd v))
  | L.Lv (L.Arr (v,e)) -> F.Read (Var (fst v, typ_to_sort (snd v)), trans_e e)
  | L.Plus (e1,e2) -> F.BinOp (F.Add, trans_e e1, trans_e e2, ESort Int)
  | L.Minus (e1,e2) -> F.BinOp (F.Sub, trans_e e1, trans_e e2, ESort Int)
  | L.Mul (e1,e2) -> F.BinOp (F.Mul, trans_e e1, trans_e e2, ESort Int)
  | _ -> failwith ("trans_e : " ^ to_string_exp exp)

and trans_v : L.var -> F.var
= fun v -> (fst v, typ_to_sort (snd v))

let rec replace_f : F.var -> F.term -> F.formula -> F.formula
= fun tar rep formula -> raise NotImplemented (* TODO *)

(* Precondition generator *)

(* aset1: "ongoing" assertion set: contains interim VCs for checking assertion safety *)
(* aset2: "completed" assertion set: contains final VCs for checking assertion safety *)
(* For example, consider the code below
   ======================================
   assert (e1);
   while [inv] (e) {S; assert (e2)};
   assert (e3)
   ======================================
   if we are processing the loop body (assuming backward analysis),
   the VC for e3 must have been added in aset2 and
   the VC for e2 must be in aset1.
*)

(* NOTE: assumption related to assertion set *)
(* Testcase programs will contain assertions that exist in a single basic path only.
   Examples:
   1) if (e1) {...} else {...} assert(e2) <- this program will not appear as testcase
   2) if (e1) {... assert(e)} else {...} <- this program may appear as testcase
 *)
(* Why is this assumption necessary?
   You can collect asets just by doing list concatenation under the assumption *)

let rec pregen : L.var * F.formula ->
                 F.formula * F.formula list * F.formula list * cmd ->
                 F.formula * F.formula list * F.formula list
= fun (rv,post) (f,aset1,aset2,cmd) ->
  match cmd with
  | Assign (L.Var a, Lv (Var b))
    when snd a = DArray Int || snd a = DArray Bool ->
    raise NotImplemented (* TODO *)
  | Assign (L.Var v,e) ->
    if is_bexp e then raise NotImplemented (* TODO *)
    else
      let tar = trans_v v in
      let rep = trans_e e in
      (replace_f tar rep f, List.map (replace_f tar rep) aset1, aset2)
  | Assign (L.Arr (v,e1), e2) -> raise NotImplemented (* TODO *)
  | Assert e -> (f, (trans_b e)::aset1, aset2)
  | _ -> raise NotImplemented (* TODO *)

(* generate VCs for checking whether loop annotations are inductive and strong *)
let rec check_inv : L.var * F.formula -> F.formula * cmd -> F.formula
= fun (rv,post) (f,cmd) ->
  match cmd with
  | Decl _ | Assign _ | Skip -> F.True
  | Seq (c1,c2) ->
    let (f',_,_) = pregen (rv,post) (f,[],[],c2) in
    F.And (check_inv (rv,post) (f', c1), check_inv (rv,post) (f,c2))
  | If (e,c1,c2) -> F.And (check_inv (rv,post) (f,c1), check_inv (rv,post) (f,c2))
  | _ -> raise NotImplemented (* TODO *)

let run : pgm -> bool * int
= fun (pre,post,fid,iparams,rparam,cmd) ->
  let pre = F.And (pre, len_non_neg) in
  let rv = snd rparam in
  let f_inv = check_inv (rv,post) (post,cmd) in
  if Solver.check_validity f_inv then
    let (f_executed,aset1,aset2) = pregen (rv,post) (post,[],[],cmd) in
    (* make final VCs for assertions between pre and the first cutpoint (loop inv or post) *)
    let aset1 = List.map (fun g -> F.Imply (pre, g)) aset1 in
    if Solver.check_validity (F.Imply (pre,f_executed)) then
      let aset = aset1 @ aset2 in
      (true, List.length (List.filter Solver.check_validity aset))
    else (false, 0) (* post or annotation of the first loop is not implied by pre *)
  else (false, 0) (* some annotations of the loops are not preserved or not strong *)
