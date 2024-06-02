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

(* collect variable components to fill in each hole *)
let rec collect : cmd -> comps * CompMap.t -> comps * CompMap.t
= fun cmd (comps,map) ->
  match cmd with
  | Decl (typ,var) ->
    (match typ with
     | ETyp Int -> ({ comps with ivars = BatSet.add var comps.ivars }, map)
     | DArray Int -> ({ comps with avars = BatSet.add var comps.avars }, map)
     | FArray (Int,_) -> ({ comps with avars = BatSet.add var comps.avars }, map)
     | _ -> (comps,map))
  | Assign _ | Skip -> (comps,map)
  | _ -> raise NotImplemented (* TODO *)

(* enumerate atomic candidate invariants from components *)
let gen_atoms : comps -> formula BatSet.t
= fun comps -> raise NotImplemented (* TODO *)

(* generate components from input parameters *)
let mk_comps0 iparams : comps =
  let comps0 = { avars = BatSet.empty; ivars = BatSet.empty } in
  List.fold_left (fun acc (typ,var) ->
    match typ with
    | ETyp Int -> { acc with ivars = BatSet.add var acc.ivars }
    | DArray Int -> { acc with avars = BatSet.add var acc.avars }
    | FArray (Int,_) -> { acc with avars = BatSet.add var acc.avars }
    | _ -> acc
  ) comps0 iparams

let run : pgm -> InvMap.t
= fun (pre,post,fid,iparams,rparam,cmd) ->
  let comps0 = mk_comps0 iparams in
  let (comps,cmap) = collect cmd (comps0, CompMap.empty) in
  print_endline ("[INFO] components collected");
  print_endline (CompMap.to_string cmap);
  let imap =
    let add k v acc = InvMap.add k (gen_atoms v) acc in
    CompMap.fold add cmap InvMap.empty in
  print_endline ("[INFO] invariant map constructed");
  print_endline (InvMap.to_string imap);
  imap
