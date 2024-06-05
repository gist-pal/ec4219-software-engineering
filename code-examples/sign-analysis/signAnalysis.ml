exception NotImplemented

(********************)
(***** Language *****)
(********************)

type aexp =
  | Int of int
  | Var of var
  | Plus of aexp * aexp
  | Mul of aexp * aexp
  | Sub of aexp * aexp

and var = string

type bexp =
  | True | False
  | Eq of aexp * aexp
  | Leq of aexp * aexp
  | Not of bexp
  | And of bexp * bexp

type cmd =
  | Assign of var * aexp
  | Skip
  | Seq of cmd * cmd
  | If of bexp * cmd * cmd
  | While of bexp * cmd

(***************************)
(***** Abstract Domain *****)
(***************************)

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

  let to_string : t -> string
  = fun t ->
    match t with
    | Bot -> "bot"
    | False -> "false"
    | True -> "true"
    | Top -> "top"

  let not : t -> t
  = fun b ->
    match b with
    | Bot -> Bot
    | True -> False
    | False -> True
    | Top -> Top

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

module Sign = struct
  type t = Top | Bot | Pos | Neg | Zero | NonPos | NonNeg | NonZero

  let porder : t -> t -> bool
  = fun s1 s2 ->
    match s1,s2 with
    | _ when s1 = s2 -> true
    | Bot,_ -> true
    | _,Top -> true
    | Neg,NonPos -> true
    | Neg,NonZero -> true
    | Zero,NonPos -> true
    | Zero,NonNeg -> true
    | Pos,NonZero -> true
    | Pos,NonNeg -> true
    | _ -> false

  let to_string : t -> string
  = fun t ->
    match t with
    | Top -> "top"
    | Bot -> "bot"
    | Pos -> "pos"
    | Neg -> "neg"
    | Zero -> "zero"
    | NonPos -> "nonpos"
    | NonNeg -> "nonneg"
    | NonZero -> "nonzero"


  let alpha' : int -> t
  = fun n ->
    if n>0 then Pos
    else if n=0 then Zero
    else Neg

  let join : t -> t -> t
  = fun s1 s2 ->
    if porder s1 s2 then s2
    else if porder s2 s1 then s1
    else
      match s1,s2 with
      | Neg,Pos 
      | Pos,Neg -> NonZero
      | Neg,Zero
      | Zero,Neg -> NonPos
      | Zero,Pos
      | Pos,Zero -> NonNeg
      | _ -> Top

  let add : t -> t -> t
  = fun s1 s2 ->
    match s1,s2 with
    | Bot,_
    | _,Bot -> Bot
    | Top,_
    | _,Top -> Top
    | Neg,Neg -> Neg
    | Neg,Zero -> Neg
    | Neg,NonPos -> Neg
    | Neg,_ -> Top
    | Zero,_ -> s2
    | Pos,Zero
    | Pos,Pos
    | Pos,NonNeg -> Pos
    | Pos,_ -> Top
    | NonPos,Neg -> Neg 
    | NonPos,Zero -> NonPos
    | NonPos,NonPos -> NonPos
    | NonPos,_ -> Top
    | NonZero,Zero -> NonZero
    | NonZero,_ -> Top
    | NonNeg,Zero -> NonNeg
    | NonNeg,Pos -> Pos
    | NonNeg,NonNeg -> NonNeg
    | NonNeg,_ -> Top

  let sub s1 s2 = raise NotImplemented (* TODO: exercise *)
  let mul s1 s2 = raise NotImplemented (* TODO: exercise *)

  let leq : t -> t -> AbsBool.t
  = fun s1 s2 ->
    match s1,s2 with
    | Bot,_ -> AbsBool.Bot
    | _,Bot -> AbsBool.Bot
    | Neg,_ when List.mem s2 [Zero;Pos;NonNeg] -> AbsBool.True
    | Zero,_ when List.mem s2 [Zero;Pos;NonNeg] -> AbsBool.True
    | Pos,_ -> AbsBool.Top
    | NonPos,_ when List.mem s2 [Pos] -> AbsBool.True
    | NonZero,_ -> AbsBool.Top
    | NonNeg,_ -> AbsBool.Top
    | _ -> AbsBool.Top

  let eq : t -> t -> AbsBool.t
  = fun s1 s2 ->
    match s1,s2 with
    | Bot,_ -> AbsBool.Bot
    | _,Bot -> AbsBool.Bot
    | Zero,_ when List.mem s2 [Neg;Pos;NonZero] -> AbsBool.False
    | Zero,Zero -> AbsBool.True
    | Pos,_ when List.mem s2 [Neg;Zero;NonPos] -> AbsBool.False
    | NonPos,Pos -> AbsBool.False
    | NonZero,Zero -> AbsBool.False
    | NonNeg,Neg -> AbsBool.False
    | _ -> AbsBool.Top
end

module AbsMem = struct
  (* reference : https://ocaml.org/manual/5.2/api/Map.Make.html *)
  module Map = Map.Make(String) (* key domain: variable *)
  type t = Sign.t Map.t (* map domain: string(var) -> Sign.t *)

  let empty = Map.empty
  let add = Map.add
  let find x m = try Map.find x m with _ -> Sign.Bot

  let join m1 m2 =
    let f k v1 v2 =
      match v1,v2 with
      | None,None -> None
      | Some v,None -> Some v
      | None,Some v -> Some v
      | Some v1,Some v2 -> Some (Sign.join v1 v2)
    in
    Map.merge f m1 m2

  let porder : t -> t -> bool
  = fun m1 m2 ->
    Map.for_all (fun x v -> Sign.porder v (find x m2)) m1

  let print : t -> unit
  = fun m ->
    if Map.is_empty m then print_endline "empty"
    else
      Map.iter (fun x v -> print_endline (x ^ " |-> " ^ Sign.to_string v)) m
end

(******************************)
(***** Abstract Semantics *****)
(******************************)

let rec eval_a : aexp -> AbsMem.t -> Sign.t
= fun a m ->
  match a with
  | Int n -> Sign.alpha' n
  | Var x -> AbsMem.find x m
  | Plus (a1, a2) -> Sign.add (eval_a a1 m) (eval_a a2 m)
  | Mul (a1, a2) -> Sign.mul (eval_a a1 m) (eval_a a2 m)
  | Sub (a1, a2) -> Sign.sub (eval_a a1 m) (eval_a a2 m)

let rec eval_b : bexp -> AbsMem.t -> AbsBool.t
= fun b m ->
  match b with
  | True -> AbsBool.True
  | False -> AbsBool.False
  | Eq (a1, a2) -> Sign.eq (eval_a a1 m) (eval_a a2 m)
  | Leq (a1, a2) -> Sign.leq (eval_a a1 m) (eval_a a2 m)
  | Not b -> AbsBool.not (eval_b b m)
  | And (b1, b2) -> AbsBool.band (eval_b b1 m) (eval_b b2 m)

let rec eval_c : cmd -> AbsMem.t -> AbsMem.t
= fun c m ->
   match c with
   | Assign (x,a) -> AbsMem.add x (eval_a a m) m
   | Seq (c1,c2) -> eval_c c2 (eval_c c1 m)
   | If (b, c1, c2) -> cond (eval_b b, eval_c c1, eval_c c2) m
   | While (b, c) ->
     let filter p x = if AbsBool.porder AbsBool.True (eval_b p x) then x else AbsMem.empty in
     let onestep x = AbsMem.join m (eval_c c (filter b x)) in
     let rec fix f x i =
       let x' = f x in
       let _ = print_endline ("=== iteration " ^ string_of_int i ^ " ===") in
       let _ = print_endline "- input memory" in
       let _ = AbsMem.print x in
       let _ = print_endline "- output memory" in
       let _ = AbsMem.print x' in
       let _ = print_endline "" in
       if AbsMem.porder x' x then x
       else fix f x' (i+1)
     in
     filter (Not b) (fix onestep m 1)

and cond (f,g,h) m =
  match f m with
  | AbsBool.Bot -> AbsMem.empty
  | AbsBool.True -> g m
  | AbsBool.False -> h m
  | AbsBool.Top -> AbsMem.join (g m) (h m)

(****************)
(***** Test *****)
(****************)

let pgm =
  let l1 = Assign ("x", Int 0) in
  let l2 = Assign ("y", Int 0) in
  let l5 = Assign ("x", Plus (Var "x", Int 1)) in
  let l8 = Assign ("x", Plus (Var "x", Var "y")) in
  let l10 = Assign ("y", Plus (Var "y", Int 1)) in
  let loop = 
    While (Leq (Var "y", Var "n"),
      Seq (If (Eq (Var "z", Int 0), l5, l8), l10)
    )
  in
  Seq (l1, Seq (l2, loop))

let mem = (AbsMem.add "z" Sign.Top (AbsMem.add "n" Sign.Top AbsMem.empty))
let _ =
  let mem' = eval_c pgm mem in
  print_endline "=== final memory ===";
  AbsMem.print mem'
