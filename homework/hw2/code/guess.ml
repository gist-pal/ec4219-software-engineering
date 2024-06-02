open Vocab
open Lang
open Formula
open Solver

module F = Formula
module L = Lang

type comps = {
  avars : L.var BatSet.t;
  ivars : L.var BatSet.t
}

let to_string_comps comps =
 "(" ^ string_of_set fst comps.avars ^ ", " ^ string_of_set fst comps.ivars ^ ")"

module type Ordered = sig
  type t
  val compare : t -> t-> int
  val to_string : t -> string
end

module MakeMap(A:Ordered) (B:Ordered) = struct
  module BatMap = BatMap.Make(A)
  include BatMap
  type t = B.t BatMap.t

  let add k v m = BatMap.add k v m
  let union m1 m2 = BatMap.fold add m2 m1 (* m2 override m1 *)

  let to_string : t -> string
  = fun x ->
    let add_string_of_k_v k v acc =
      let str = A.to_string k ^ " -> " ^ B.to_string v in
      if acc = "" then str else acc ^ ",\n" ^ str in
    if BatMap.is_empty x then "empty"
    else "{" ^ BatMap.fold add_string_of_k_v x "" ^ "}"
end

module CompMap = struct
  module A = struct
    type t = hid
    let compare = Stdlib.compare
    let to_string a = "#H" ^ string_of_int a
  end
  module B = struct
    type t = comps
    let compare = Stdlib.compare
    let to_string = to_string_comps
  end
  module Map = MakeMap (A)(B)
  include Map
  type t = Map.t

  let add k v m =
    try
      let _ = Map.find k m in
      failwith "CompMap: add op should happen only once"
    with Not_found -> Map.add k v m
end

module InvMap = struct
  module A = struct
    type t = hid
    let compare = Stdlib.compare
    let to_string a = "#H" ^ string_of_int a
  end
  module B = struct
    type t = formula BatSet.t
    let compare = Stdlib.compare
    let to_string = string_of_set to_string_formula
  end
  module Map = MakeMap (A)(B)
  include Map
  type t = Map.t
end

let run : pgm -> InvMap.t
= fun (pre,post,fid,iparams,rparam,cmd) ->
  raise NotImplemented (* TODO *)
