open Pgm
open Formula
open Solver
open Verify_utils

exception NotImplemented

let gen_vc : pgm -> int -> formula
= fun pgm cur_bound -> raise NotImplemented (* TODO *)


type res = int * Z3.Model.model

let run : pgm -> res option
= fun pgm ->
  let rec loop (cur_bound:int) : res option =
    if cur_bound = !Options.max_bound then None
    else begin
      Printf.eprintf "[INFO] current bound: %d\n%!" cur_bound;
      assert (0 < cur_bound && cur_bound < !Options.max_bound);

      let vc = gen_vc pgm cur_bound in
      match Solver.check_sat vc with
      | SAT,Some model -> Some (cur_bound,model)
      | UNSAT,None -> loop (cur_bound + 1)
      | UNK,None -> failwith "unknown results"
      | _ -> assert false
    end
  in
  loop 1
