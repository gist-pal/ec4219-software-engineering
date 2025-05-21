open Formula
open Pgm

(* X_t : a shared variable X at time t *)
let v2 (id,t) =
  Var (id ^ "_" ^ string_of_int t)

(* H_lt : thread H is in state l at time t *)
let v3 (tid,l,t) =
  Var (tid ^ "_" ^ string_of_int l ^ string_of_int t)

(* A_lt : thread A is in state l at time t *)
let a (l,t) = v3 ("A",l,t)

(* B_lt : thread B is in state l at time t *)
let b (l,t) = v3 ("B",l,t)
