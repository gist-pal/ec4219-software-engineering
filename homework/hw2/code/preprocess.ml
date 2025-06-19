open Term
open Lang
open Utils

module type Ordered = sig
  type t
  val compare : t -> t-> int
  val to_string : t -> string
end

module MakeMap(A:Ordered) (B:Ordered) = struct
  module BatMap = BatMap.Make(A)
  include BatMap
  type t = B.t BatMap.t
  let to_string : t -> string
  = fun x ->
    let add_string_of_k_v k v acc =
      let str = A.to_string k ^ " -> " ^ B.to_string v in
      if acc = "" then str else acc ^ ",\n" ^ str in
    if BatMap.is_empty x then "empty"
    else "{" ^ BatMap.fold add_string_of_k_v x "" ^ "}"
end

module TyMap = struct
  module A = struct type t = vid let compare = Stdlib.compare let to_string x = x end
  module B = struct type t = typ let compare = Stdlib.compare let to_string = to_string_typ end
  module Map = MakeMap (A)(B)
  include Map
  type t = Map.t

  let add k v m =
    try
      let v' = Map.find k m in
      if v = v' then m
      else failwith (A.to_string k ^ " is not well-typed : " ^ B.to_string v ^ " vs. " ^ B.to_string v')
    with Not_found -> Map.add k v m
  let union m1 m2 = Map.fold add m2 m1
end

(* Assumes bound vars have either int or darray types. *)
let sort_to_typ : sort -> typ
= fun sort ->
  match sort with
  | S_Int -> Ty_Basic (Ty_Int)
  | S_Arr (S_Int, S_Int) -> Ty_DArr (Ty_Int)
  | _ -> assert false

let prop_v_lang : TyMap.t -> L.var -> L.var
= fun tmap (x,_) -> (x, TyMap.find x tmap)

let prop_v_term : TyMap.t -> T.var -> T.var
= fun tmap (x,_) -> (x, TyMap.find x tmap |> typ_to_sort)


let rec prop_t : TyMap.t -> T.term -> T.term
= fun m term ->
  match term with
  | True | False -> term
  | Not t -> Not (prop_t m t)
  | And (t1,t2) -> And (prop_t m t1, prop_t m t2)
  | Or (t1,t2) -> Or (prop_t m t1, prop_t m t2)
  | BinRel (brel,t1,t2) -> BinRel (brel, prop_t m t1, prop_t m t2)
  | Imply (t1,t2) -> Imply (prop_t m t1, prop_t m t2)
  | Iff (t1,t2) -> Iff (prop_t m t1, prop_t m t2)
  | Forall (bvars,t) ->
    let m' =
      let step acc (x,sort) = TyMap.add x (sort_to_typ sort) acc in
      List.fold_left step m bvars
    in
    Forall (bvars, prop_t m' t)
  | Exists (bvars,t) ->
    let m' =
      let step acc (x,sort) = TyMap.add x (sort_to_typ sort) acc in
      List.fold_left step m bvars
    in
    Exists (bvars, prop_t m' t)
  | Sorted (t1,t2,t3) -> Sorted (prop_t m t1, prop_t m t2, prop_t m t3)
  | Partitioned (t1,t2,t3,t4,t5) ->
    Partitioned (prop_t m t1, prop_t m t2, prop_t m t3, prop_t m t4, prop_t m t5)
  | Int n -> term
  | Var v -> Var (prop_v_term m v)
  | Len v -> Len (prop_v_term m v)
  | BinOp (bop,t1,t2) -> BinOp (bop, prop_t m t1, prop_t m t2)
  | Read (t1,t2) -> Read (prop_t m t1, prop_t m t2)
  | Write (t1,t2,t3) -> Write (prop_t m t1, prop_t m t2, prop_t m t3)


let rec prop_c : TyMap.t -> cmd -> TyMap.t * cmd
= fun m cmd ->
  match cmd with
  | Decl x -> (TyMap.add (fst x) (snd x) m, cmd)
  | Assign (lv,e) -> (m, Assign (prop_lv m lv, prop_e m e))
  | Skip -> (m, cmd)
  | Seq (c1,c2) ->
    let (m',c1') = prop_c m c1 in
    let (m'',c2') = prop_c m' c2 in
    (m'', Seq (c1',c2'))
  | If (e,c1,c2) ->
    let e' = prop_e m e in
    let (_,c1') = prop_c m c1 in
    let (_,c2') = prop_c m c2 in
    (m, If (e',c1',c2'))
  | While (inv,e,c) ->
    let inv' = prop_t m inv in
    let e' = prop_e m e in
    let (_,c') = prop_c m c in
    (m, While (inv',e',c'))
  | Assert (l,e) -> (m, Assert (l,prop_e m e))
  | Return e -> (m, Return (prop_e m e))

and prop_e : TyMap.t -> exp -> exp
= fun m exp ->
  match exp with
  | Int _ -> exp
  | Lv lv -> Lv (prop_lv m lv)
  | Len v -> Len (prop_v_lang m v)
  | BinOp (bop,e1,e2) -> BinOp (bop, prop_e m e1, prop_e m e2)
  | True | False -> exp
  | BinRel (brel,e1,e2) -> BinRel (brel, prop_e m e1, prop_e m e2)
  | Not e -> Not (prop_e m e)
  | Or (e1,e2) -> Or (prop_e m e1, prop_e m e2)
  | And (e1,e2) -> And (prop_e m e1, prop_e m e2)

and prop_lv : TyMap.t -> lv -> lv
= fun m lv ->
  match lv with
  | Var v -> Var (prop_v_lang m v)
  | ArrAccess (v,e) ->
    let v' = prop_v_lang m v in
    let e' = prop_e m e in
    ArrAccess (v',e')

let propagate tymap (pre, post, fid, iparams, rparam, cmd) =
  let pre' = prop_t tymap pre in
  let post' = prop_t tymap post in
  let (_,cmd') = prop_c tymap cmd in
  (pre', post', fid, iparams, rparam, cmd')

let add_constraint (pre,post,fid,iparams,rparam,cmd) =
  (* common function precondition: ∀a. len(a) >= 0 *)
  let len_is_non_neg =
    let a = ("@a", S_Int) in
    Forall ([a],
      BinRel (Geq, Read (Var len_smt, Var a), Int 0)
    )
  in
  let pre' = T.And (pre,len_is_non_neg) in
  (pre',post,fid,iparams,rparam,cmd)

let run ((_,_,_,iparams,rparam,_) as pgm) : pgm =
  let tymap =
    let step acc (x,typ) = TyMap.add x typ acc in
    List.fold_left step TyMap.empty (rparam::iparams)
  in
  pgm |> propagate tymap |> add_constraint
