open Z3
open Z3.Solver
open Formula

let ctx = Z3.mk_context [("timeout", "30000")]

let rec to_z3sort : sort -> Z3.Sort.sort
= fun sort ->
  match sort with
  | ESort et -> to_z3esort et
  | Array (s1,s2) ->
    Z3.Z3Array.mk_sort ctx (to_z3sort s1) (to_z3sort s2)
  | NullSort -> assert false

and to_z3esort : esort -> Z3.Sort.sort
= fun esort ->
  match esort with
  | Int -> Z3.Arithmetic.Integer.mk_sort ctx
  | Bool -> Z3.Boolean.mk_sort ctx

let rec trans_f : formula -> Z3.Expr.expr
= fun f -> 
  match f with
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | PVar x -> Z3.Boolean.mk_const_s ctx x
  | Sorted (a,l,u) ->
    let i = ("@i", ESort Int) in
    let j = ("@j", ESort Int) in
    let pre1 = BinRel (Leq, l, Var i) in
    let pre2 = BinRel (Leq, Var i, Var j) in
    let pre3 = BinRel (Leq, Var j, u) in
    let con = BinRel (Leq, Read (a, Var i), Read (a, Var j)) in
    let final = Forall ([i;j], Imply (And (pre1, And (pre2,pre3)), con)) in
    trans_f final
  | Partitioned (a,l1,u1,l2,u2) ->
    let i = ("@i", ESort Int) in
    let j = ("@j", ESort Int) in
    let pre1 = BinRel (Leq, l1, Var i) in
    let pre2 = BinRel (Leq, Var i, u1) in
    let pre3 = BinRel (Lt, u1, l2) in
    let pre4 = BinRel (Leq, l2, Var j) in
    let pre5 = BinRel (Leq, Var j, u2) in
    let pre = And (pre1, And (pre2, And (pre3, And (pre4, pre5)))) in
    let con = BinRel (Leq, Read (a, Var i), Read (a, Var j)) in
    let final = Forall ([i;j], Imply (pre, con)) in
    trans_f final
  | Not f -> Z3.Boolean.mk_not ctx (trans_f f)
  | And (f1,f2) -> Z3.Boolean.mk_and ctx [trans_f f1; trans_f f2]
  | Or (f1,f2) -> Z3.Boolean.mk_or ctx [trans_f f1; trans_f f2]
  | BinRel (brel,t1,t2) ->
   let z3exp1 = trans_t t1 in
   let z3exp2 = trans_t t2 in
    (match brel with
     | Geq -> Z3.Arithmetic.mk_ge ctx z3exp1 z3exp2
     | Gt -> Z3.Arithmetic.mk_gt ctx z3exp1 z3exp2
     | Leq -> Z3.Arithmetic.mk_le ctx z3exp1 z3exp2
     | Lt -> Z3.Arithmetic.mk_lt ctx z3exp1 z3exp2
     | Eq -> Z3.Boolean.mk_eq ctx z3exp1 z3exp2
     | Neq -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3exp1 z3exp2))
  | Imply (f1,f2) -> Z3.Boolean.mk_implies ctx (trans_f f1) (trans_f f2)
  | Iff (f1,f2) -> Z3.Boolean.mk_iff ctx (trans_f f1) (trans_f f2)
  | Forall (vars,f) ->
    let bvars = List.map (fun (x,t) -> trans_t (Var (x,t))) vars in
    let z3f = trans_f f in
    Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall_const ctx bvars z3f None [] [] None None)
  | Exists (vars,f) ->
    let bvars = List.map (fun (x,t) -> trans_t (Var (x,t))) vars in
    let z3f = trans_f f in
    Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_exists_const ctx bvars z3f None [] [] None None)
  | Hole _ -> failwith "solver.ml: Holes should not appear"

and trans_t : term -> Z3.Expr.expr
= fun t ->
  match t with
  | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
  | Var (x,sort) -> Z3.Expr.mk_const_s ctx x (to_z3sort sort)
  | Len v ->
    let len = ("len", Array (ESort Int, ESort Int)) in
    trans_t (Read (Var len, Var (fst v, ESort Int)))
  | Read (t1,t2) ->
    let z3exp1 = trans_t t1 in
    let z3exp2 = trans_t t2 in
    Z3.Z3Array.mk_select ctx z3exp1 z3exp2
  | Write (t1,t2,t3) ->
    let z3exp1 = trans_t t1 in
    let z3exp2 = trans_t t2 in
    let z3exp3 = trans_t t3 in
    Z3.Z3Array.mk_store ctx z3exp1 z3exp2 z3exp3
  | BinOp (bop,t1,t2,typ) ->
    let _ = assert (typ = ESort Int) in
    let z3exp1 = trans_t t1 in
    let z3exp2 = trans_t t2 in
    (match bop with
     | Add -> Z3.Arithmetic.mk_add ctx [z3exp1; z3exp2]
     | Sub -> Z3.Arithmetic.mk_sub ctx [z3exp1; z3exp2]
     | Mul -> Z3.Arithmetic.mk_mul ctx [z3exp1; z3exp2])

(*********************)
(**** Z3 wrappers ****)
(*********************)

let mk_solver : unit -> Z3.Solver.solver
= fun () -> Z3.Solver.mk_solver ctx None

type status =
  | SAT
  | UNSAT
  | UNK

let check_sat : formula -> (status * Z3.Model.model option)
= fun f ->
  let solver = mk_solver () in
  let _ = Z3.Solver.add solver [trans_f f] in
  (match Z3.Solver.check solver [] with
   | UNSATISFIABLE -> (UNSAT, None)
   | UNKNOWN -> (UNK, None)
   | SATISFIABLE ->
     (match Z3.Solver.get_model solver with
       | Some m -> (SAT, Some m)
       | None -> assert false))

let check_validity : formula -> bool
= fun f ->
  match check_sat (Not f) with
  | UNSAT,_ -> true
  | _ -> false

let string_of_model : Z3.Model.model -> string
= fun m -> Z3.Model.to_string m

let check_sat_print : formula -> unit
= fun f ->
  match check_sat f with
  | SAT, Some m ->
    (print_endline "SAT";
     print_endline (string_of_model m))
  | SAT,None -> assert false
  | UNSAT,_ -> print_endline "UNSAT"
  | UNK,_ -> print_endline "UNKNOWN"
