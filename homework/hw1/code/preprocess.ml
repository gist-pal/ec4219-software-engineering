open Vocab
open Formula
open Lang

module F = Formula
module L = Lang

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

module TypMap = struct
  module A = struct type t = vid let compare = Stdlib.compare let to_string = id end
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

let rec typ_to_sort : typ -> sort
= fun typ ->
  match typ with
  | ETyp et -> ESort (etyp_to_esort et)
  | DArray et -> Array (ESort Int, ESort (etyp_to_esort et))
  | FArray (et,_) -> Array (ESort Int, ESort (etyp_to_esort et))
  | NullTyp -> failwith "preprocess.ml : NullTyp"

and etyp_to_esort : etyp -> esort
= fun etyp ->
  match etyp with
  | L.Int -> F.Int
  | L.Bool -> F.Bool

let sort_to_typ : sort -> typ
= fun sort ->
  match sort with
  | ESort Int -> ETyp Int
  | Array (ESort Int, ESort Int) -> DArray Int
  | _ -> assert false

let add_v2 : TypMap.t -> F.var -> F.var
 =fun tmap (x,_) -> (x, TypMap.find x tmap |> typ_to_sort)

let rec add_f : TypMap.t -> formula -> formula
= fun m f ->
  match f with
  | F.True | F.False | F.PVar _ -> f
  | F.Sorted (t1,t2,t3) -> F.Sorted (add_t m t1, add_t m t2, add_t m t3)
  | F.Partitioned (t1,t2,t3,t4,t5) ->
    F.Partitioned (add_t m t1, add_t m t2, add_t m t3, add_t m t4, add_t m t5)
  | F.Not f -> F.Not (add_f m f)
  | F.And (f1,f2) -> F.And (add_f m f1, add_f m f2)
  | F.Or (f1,f2) -> F.Or (add_f m f1, add_f m f2)
  | F.BinRel (brel,t1,t2) -> F.BinRel (brel, add_t m t1, add_t m t2)
  | F.Imply (f1,f2) -> F.Imply (add_f m f1, add_f m f2)
  | F.Iff (f1,f2) -> F.Iff (add_f m f1, add_f m f2)
  | F.Forall (bvars,f) ->
    let m' = List.fold_left (fun acc (x,s) -> TypMap.add x (sort_to_typ s) acc) m bvars in
    F.Forall (bvars, add_f m' f)
  | F.Exists (bvars,f) ->
    let m' = List.fold_left (fun acc (x,s) -> TypMap.add x (sort_to_typ s) acc) m bvars in
    F.Exists (bvars, add_f m' f)

and add_t : TypMap.t -> term -> term
= fun tmap term ->
  match term with
  | F.Int _ -> term
  | F.Var v -> F.Var (add_v2 tmap v)
  | F.Len v -> F.Len (add_v2 tmap v)
  | F.Read (t1,t2) -> F.Read (add_t tmap t1, add_t tmap t2)
  | F.Write (t1,t2,t3) -> F.Write (add_t tmap t1, add_t tmap t2, add_t tmap t3)
  | F.BinOp (bop,t1,t2,typ) -> F.BinOp (bop, add_t tmap t1, add_t tmap t2, typ)

let add_v : TypMap.t -> var -> var
= fun tmap (x,_) -> (x, TypMap.find x tmap)

let rec add_lv : TypMap.t -> lv -> lv
= fun tmap lv ->
  match lv with
  | L.Var v -> L.Var (add_v tmap v)
  | L.Arr (v,e) -> L.Arr (add_v tmap v, add_e tmap e)

and add_e : TypMap.t -> exp -> exp
= fun tmap exp ->
  match exp with
  | L.Int _ -> exp
  | L.Len v -> L.Len (add_v tmap v)
  | L.Lv lv -> L.Lv (add_lv tmap lv)
  | L.Plus (e1,e2) -> L.Plus (add_e tmap e1, add_e tmap e2)
  | L.Minus (e1,e2) -> L.Minus (add_e tmap e1, add_e tmap e2)
  | L.Mul (e1,e2) -> L.Mul (add_e tmap e1, add_e tmap e2)
  | L.True | L.False -> exp
  | L.Eq (e1,e2) -> L.Eq (add_e tmap e1, add_e tmap e2)
  | L.Neq (e1,e2) -> L.Neq (add_e tmap e1, add_e tmap e2)
  | L.Leq (e1,e2) -> L.Leq (add_e tmap e1, add_e tmap e2)
  | L.Lt (e1,e2) -> L.Lt (add_e tmap e1, add_e tmap e2)
  | L.Geq (e1,e2) -> L.Geq (add_e tmap e1, add_e tmap e2)
  | L.Gt (e1,e2) -> L.Gt (add_e tmap e1, add_e tmap e2)
  | L.Not e -> L.Not (add_e tmap e)
  | L.Or (e1,e2) -> L.Or (add_e tmap e1, add_e tmap e2)
  | L.And (e1,e2) -> L.And (add_e tmap e1, add_e tmap e2)

let rec add_c : TypMap.t -> cmd -> TypMap.t * cmd
= fun tmap cmd ->
  match cmd with
  | Decl (t,x) -> (TypMap.add (fst x) t tmap, cmd)
  | Assign (lv,e) -> (tmap, Assign (add_lv tmap lv, add_e tmap e))
  | Skip -> (tmap, cmd)
  | Seq (c1,c2) ->
    let (tmap',c1') = add_c tmap c1 in
    let (tmap'',c2') = add_c tmap' c2 in
    (tmap'', Seq(c1',c2'))
  | If (e,c1,c2) -> (tmap, If (add_e tmap e, snd (add_c tmap c1), snd (add_c tmap c2)))
  | While (inv,e,c) -> (tmap, While (add_f tmap inv, add_e tmap e, snd (add_c tmap c)))
  | Assert e -> (tmap, Assert (add_e tmap e))
  | Return e -> (tmap, Return (add_e tmap e))

let update t0 (a,b,c,d,e,f) = (add_f t0 a, add_f t0 b,c,d,e, snd (add_c t0 f))

let run ((_,_,_,iparams,rparam,_) as pgm) =
  let tmap0 =
    List.fold_left (fun acc (t,(x,_)) -> TypMap.add x t acc) TypMap.empty (rparam::iparams) in
  update tmap0 pgm
