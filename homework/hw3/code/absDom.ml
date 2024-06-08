open Lang
open Vocab

module AbsBool = struct
  type t = Top | Bot | True | False

  let porder : t -> t -> bool
  = fun b1 b2 ->
    if b1 = b2 then true
    else
      match b1,b2 with
      | Bot,_ -> true
      | _,Top -> true
      | _ -> false

  let join : t -> t -> t
  = fun b1 b2 ->
    if porder b1 b2 then b2
    else if porder b2 b1 then b1
    else
      match b1,b2 with
      | True,False
      | False,True -> Top
      | _ -> assert false

  let alpha' : bool -> t
  = fun b -> if b then True else False

  let to_string : t -> string
  = fun t ->
    match t with
    | Bot -> "bot"
    | False -> "false"
    | True -> "true"
    | Top -> "top"

  let bnot : t -> t
  = fun b ->
    match b with
    | Bot -> Bot
    | True -> False
    | False -> True
    | Top -> Top

  let eq : t -> t -> t
  = fun b1 b2 ->
    match b1,b2 with
    | Bot,_
    | _,Bot -> Bot
    | True,True -> True
    | False,False -> False
    | _ -> Top

  let neq : t -> t -> t
  = fun b1 b2 -> bnot (eq b1 b2)

  let bor : t -> t -> t
  = fun b1 b2 -> raise NotImplemented (* TODO *)

  let band : t -> t -> t
  = fun b1 b2 ->
    match b1,b2 with
    | Bot,_
    | _,Bot -> Bot
    | True,True -> True
    | False,_
    | _,False -> False
    | _ -> Top
end

module Itv = struct
  type t' =
    | V of int
    | PInf
    | NInf

  type t = Itv of t' * t' | Bot

  let top = Itv (NInf, PInf)
  let bot = Bot

  let alpha' : int -> t
  = fun n -> Itv (V n, V n)

  let to_string' : t' -> string
  = fun v ->
    match v with
    | V n -> string_of_int n
    | PInf -> "+oo"
    | NInf -> "-oo"

  let to_string : t -> string
  = fun itv ->
    match itv with
    | Itv (l,u) -> "[" ^ (to_string' l) ^ ", " ^ (to_string' u) ^"]"
    | Bot -> "bot"

  let leq' : t' -> t' -> bool
  = fun v1 v2 ->
    match v1,v2 with
    | NInf,_ -> true
    | _,PInf -> true
    | V n1,V n2 -> n1<=n2
    | _,_ -> false
  
  let eq' : t' -> t' -> bool
  = fun v1 v2 ->
    match v1,v2 with
    | NInf,NInf
    | PInf,PInf -> true
    | V n1,V n2 -> n1=n2
    | _,_ -> false
  
  let lt' : t' -> t' -> bool
  = fun v1 v2 -> leq' v1 v2 && not (eq' v1 v2)
  
  let gt' : t' -> t' -> bool
  = fun v1 v2 -> not (leq' v1 v2)
  
  let ge' : t' -> t' -> bool
  = fun v1 v2 -> not (lt' v1 v2) 
  
  let min' : t' -> t' -> t'
  = fun v1 v2 -> if leq' v1 v2 then v1 else v2
  
  let max' : t' -> t' -> t'
  = fun v1 v2 -> if leq' v1 v2 then v2 else v1 
  
  let plus' : t' -> t' -> t'
  = fun v1 v2 ->
    match v1,v2 with
    | V n1,V n2 -> V (n1+n2)
    | PInf,NInf
    | NInf,PInf -> assert false
    | PInf,_ -> PInf
    | NInf,_ -> NInf
    | _,PInf -> PInf
    | _,NInf -> NInf
 
  let minus' : t' -> t' -> t'
  = fun v1 v2 ->
    match v1,v2 with
    | V n1,V n2 -> V (n1-n2)
    | PInf,PInf
    | NInf,NInf -> assert false
    | PInf,_ -> PInf
    | NInf,_ -> NInf
    | _,PInf -> NInf
    | _,NInf -> PInf 
  
  let mul' : t' -> t' -> t'
  = fun v1 v2 ->
    match v1,v2 with 
    | V n1,V n2 -> V (n1*n2)
    | PInf,PInf
    | NInf,NInf -> PInf
    | PInf,NInf
    | NInf,PInf -> NInf
    | PInf,V n
    | V n,PInf ->
      if n>0 then PInf
      else if n<0 then NInf
      else V 0
    | NInf,V n
    | V n,NInf ->
      if n>0 then NInf
      else if n<0 then PInf
      else V 0

  let is_bot : t -> bool
  = fun itv ->
    match itv with
    | Bot -> true
    | Itv (l,u) -> l = PInf || u = NInf || not (leq' l u)

  let porder : t -> t -> bool
  = fun itv1 itv2 ->
    if is_bot itv1 then true
    else if is_bot itv2 then false
    else
      match itv1,itv2 with
      | Itv (l1,u1),Itv (l2,u2) -> leq' l2 l1 && leq' u1 u2
      | _ -> assert false

  (* binary least upper bound *)
  let join : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let widen : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let narrow : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  (*************************************)
  (*** Abstract Arithmetic Operators ***)
  (*************************************)
  let plus : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let minus : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)
  
  let mul : t -> t -> t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  (*********************************)
  (*** Abstract Binary Relations ***)
  (*********************************)
  let eq : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let neq : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let leq : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let lt : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let geq : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

  let gt : t -> t -> AbsBool.t
  = fun itv1 itv2 -> raise NotImplemented (* TODO *)

end

module AbsVal = struct
  type t = Itv.t * AbsBool.t

  let bot = (Itv.Bot, AbsBool.Bot)

  let to_string (i,b) = "(" ^ Itv.to_string i ^ "," ^ AbsBool.to_string b ^ ")"

  let porder (i1,b1) (i2,b2) = Itv.porder i1 i2 && AbsBool.porder b1 b2

  let join (i1,b1) (i2,b2) = raise NotImplemented (* TODO *)
  let widen (i1,b1) (i2,b2) = raise NotImplemented (* TODO *)
  let narrow (i1,b1) (i2,b2) = raise NotImplemented (* TODO *)

  let plus (i1,b1) (i2,b2) = (Itv.plus i1 i2, AbsBool.Bot)
  let minus (i1,b1) (i2,b2) = (Itv.minus i1 i2, AbsBool.Bot)
  let mul (i1,b1) (i2,b2) = (Itv.mul i1 i2, AbsBool.Bot)

  let eq (i1,b1) (i2,b2) = raise NotImplemented (* TODO *)
  let neq (i1,b1) (i2,b2) = raise NotImplemented (* TODO *)

  let leq (i1,b1) (i2,b2) = (Itv.Bot, Itv.leq i1 i2)
  let lt (i1,b1) (i2,b2) = (Itv.Bot, Itv.lt i1 i2)
  let geq (i1,b1) (i2,b2) = (Itv.Bot, Itv.geq i1 i2)
  let gt (i1,b1) (i2,b2) = (Itv.Bot, Itv.gt i1 i2)

  let bnot (i,b) = (Itv.Bot, AbsBool.bnot b)
  let bor (i1,b1) (i2,b2) = (Itv.Bot, AbsBool.bor b1 b2)
  let band (i1,b1) (i2,b2) = (Itv.Bot, AbsBool.band b1 b2)
end


module AbsMem = struct
  module Variable = struct type t = var let compare = Stdlib.compare end
  module Map = Map.Make(Variable) (* key domain: variable *)
  type t = AbsVal.t Map.t (* map domain: var -> AbsVal.t *)

  let empty = Map.empty
  let add = Map.add
  let find x m = try Map.find x m with _ -> AbsVal.bot

  let join : t -> t -> t
  = fun m1 m2 ->
    let f k v1 v2 =
      match v1,v2 with
      | None,None -> None
      | Some v,None -> Some v
      | None,Some v -> Some v
      | Some v1,Some v2 -> Some (AbsVal.join v1 v2)
    in
    Map.merge f m1 m2

  let widen : t -> t -> t
  = fun m1 m2 ->
    let f k v1 v2 =
      match v1,v2 with
      | None,None -> None
      | Some v,None -> Some v
      | None,Some v -> Some v
      | Some v1,Some v2 -> Some (AbsVal.widen v1 v2)
    in
    Map.merge f m1 m2

  let narrow : t -> t -> t
  = fun m1 m2 ->
    let f k v1 v2 =
      match v1,v2 with
      | None,None -> None
      | Some v,None -> Some v
      | None,Some v -> Some v
      | Some v1,Some v2 -> Some (AbsVal.narrow v1 v2)
    in
    Map.merge f m1 m2

  let porder : t -> t -> bool
  = fun m1 m2 -> Map.for_all (fun x v -> AbsVal.porder v (find x m2)) m1

  let print : t -> unit
  = fun m ->
    if Map.is_empty m then print_endline "empty"
    else
      Map.iter (fun x v -> print_endline (fst x ^ " |-> " ^ AbsVal.to_string v)) m
end
