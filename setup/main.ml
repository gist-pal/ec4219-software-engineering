open Z3

let () =
  let ctx = Z3.mk_context [] in
  let a = Boolean.mk_const_s ctx "a" in
  let b = Boolean.mk_const_s ctx "b" in
  let f = Boolean.mk_and ctx [a; b] in
  let solver = Solver.mk_solver ctx None in 
  match (Solver.check solver [f]) with
  | SATISFIABLE ->
    (print_endline "SAT"; 
     match Solver.get_model solver with
     | Some m -> print_endline (Model.to_string m)
     | None -> assert false)
  | UNSATISFIABLE -> print_endline "UNSAT"
  | UNKNOWN -> print_endline "UNKNOWN"
