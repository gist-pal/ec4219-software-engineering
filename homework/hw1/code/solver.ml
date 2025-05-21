open Z3
open Z3.Solver
open Formula

let ctx = Z3.mk_context [("timeout", "30000")]

let rec trans : formula -> Z3.Expr.expr
= fun f -> 
  match f with
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | Var x -> Z3.Boolean.mk_const_s ctx x
  | Not f -> Z3.Boolean.mk_not ctx (trans f)
  | And (f1,f2) -> Z3.Boolean.mk_and ctx [trans f1; trans f2]
  | Or (f1,f2) -> Z3.Boolean.mk_or ctx [trans f1; trans f2]
  | Imply (f1,f2) -> Z3.Boolean.mk_implies ctx (trans f1) (trans f2)
  | Iff (f1,f2) -> Z3.Boolean.mk_iff ctx (trans f1) (trans f2)

(*********************)
(**** Z3 wrappers ****)
(*********************)

let mk_solver : unit -> Z3.Solver.solver
= fun () -> Z3.Solver.mk_solver ctx None

type status = SAT | UNSAT | UNK

let check_sat : formula -> (status * Z3.Model.model option)
= fun f ->
  let solver = mk_solver () in
  let _ = Z3.Solver.add solver [trans f] in
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

let is_true: Z3.Model.model -> formula -> bool
= fun model f ->
  match Z3.Model.eval model (trans f) false with
  | Some z3exp -> Z3.Boolean.is_true z3exp
  | None -> assert false
