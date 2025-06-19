open Z3
open Z3.Solver
open Term
open Utils

let ctx = Z3.mk_context [("timeout", "30000")]

let rec to_z3sort : sort -> Z3.Sort.sort
= fun sort ->
  match sort with
  | S_Int -> Z3.Arithmetic.Integer.mk_sort ctx
  | S_Bool -> Z3.Boolean.mk_sort ctx
  | S_Arr (s1,s2) -> Z3.Z3Array.mk_sort ctx (to_z3sort s1) (to_z3sort s2)
  | S_Null -> assert false

let rec trans : T.term -> Z3.Expr.expr
= fun term ->
  match term with
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | Not t -> Z3.Boolean.mk_not ctx (trans t)
  | And (t1,t2) -> Z3.Boolean.mk_and ctx [trans t1; trans t2]
  | Or (t1,t2) -> Z3.Boolean.mk_or ctx [trans t1; trans t2]
  | BinRel (brel,t1,t2) ->
    let z3exp1 = trans t1 in
    let z3exp2 = trans t2 in
    (match brel with
     | Geq -> Z3.Arithmetic.mk_ge ctx z3exp1 z3exp2
     | Gt -> Z3.Arithmetic.mk_gt ctx z3exp1 z3exp2
     | Leq -> Z3.Arithmetic.mk_le ctx z3exp1 z3exp2
     | Lt -> Z3.Arithmetic.mk_lt ctx z3exp1 z3exp2
     | Eq -> Z3.Boolean.mk_eq ctx z3exp1 z3exp2
     | Neq -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3exp1 z3exp2))
  | Imply (t1,t2) -> Z3.Boolean.mk_implies ctx (trans t1) (trans t2)
  | Iff (t1,t2) -> Z3.Boolean.mk_iff ctx (trans t1) (trans t2)
  | Forall (bvs,f) ->
    let z3bvs = List.map (fun (x,typ) -> trans (Var (x,typ))) bvs in
    let z3f = trans f in
    Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall_const ctx z3bvs z3f None [] [] None None)
  | Exists (bvs,f) ->
    let z3bvs = List.map (fun (x,typ) -> trans (Var (x,typ))) bvs in
    let z3f = trans f in
    Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_exists_const ctx z3bvs z3f None [] [] None None)
  | Sorted (a,l,u) ->
    let i = ("@i", S_Int) in
    let j = ("@j", S_Int) in
    let pre1 = BinRel (Leq, l, Var i) in
    let pre2 = BinRel (Leq, Var i, Var j) in
    let pre3 = BinRel (Leq, Var j, u) in
    let con = BinRel (Leq, Read (a, Var i), Read (a, Var j)) in
    let final = Forall ([i;j], Imply (And (pre1, And (pre2,pre3)), con)) in
    trans final
  | Partitioned (a,l1,u1,l2,u2) ->
    let i = ("@i", S_Int) in
    let j = ("@j", S_Int) in
    let pre1 = BinRel (Leq, l1, Var i) in
    let pre2 = BinRel (Leq, Var i, u1) in
    let pre3 = BinRel (Lt, u1, l2) in
    let pre4 = BinRel (Leq, l2, Var j) in
    let pre5 = BinRel (Leq, Var j, u2) in
    let pre = And (pre1, And (pre2, And (pre3, And (pre4, pre5)))) in
    let con = BinRel (Leq, Read (a, Var i), Read (a, Var j)) in
    let final = Forall ([i;j], Imply (pre, con)) in
    trans final
  | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
  | Var (x,sort) -> Z3.Expr.mk_const_s ctx x (to_z3sort sort)
  | Len v -> trans (Read (Var len_smt, Var (fst v, S_Int)))
  | BinOp (bop,t1,t2) ->
    let z3exp1 = trans t1 in
    let z3exp2 = trans t2 in
    (match bop with
     | Plus  -> Z3.Arithmetic.mk_add ctx [z3exp1; z3exp2]
     | Minus -> Z3.Arithmetic.mk_sub ctx [z3exp1; z3exp2]
     | Mul   -> Z3.Arithmetic.mk_mul ctx [z3exp1; z3exp2])
  | Read (t1,t2) ->
    let z3exp1 = trans t1 in
    let z3exp2 = trans t2 in
    Z3.Z3Array.mk_select ctx z3exp1 z3exp2
  | Write (t1,t2,t3) ->
    let z3exp1 = trans t1 in
    let z3exp2 = trans t2 in
    let z3exp3 = trans t3 in
    Z3.Z3Array.mk_store ctx z3exp1 z3exp2 z3exp3

(*********************)
(**** Z3 wrappers ****)
(*********************)

let mk_solver : unit -> Z3.Solver.solver
= fun () -> Z3.Solver.mk_solver ctx None

type status = SAT | UNSAT | UNK

let check_sat : T.formula -> (status * Z3.Model.model option)
= fun f ->
  let solver = mk_solver () in
  Z3.Solver.add solver [trans f];
  match Z3.Solver.check solver [] with
  | UNSATISFIABLE -> (UNSAT, None)
  | UNKNOWN -> (UNK, None)
  | SATISFIABLE -> begin
      match Z3.Solver.get_model solver with
      | Some m -> (SAT, Some m)
      | None -> assert false
    end

let check_valid : T.formula -> bool
= fun f ->
  match check_sat (Not f) with
  | UNSAT,_ -> true
  | _ -> false

let string_of_model : Z3.Model.model -> string
= fun m -> Z3.Model.to_string m
