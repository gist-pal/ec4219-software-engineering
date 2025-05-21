open Solver
open Pgm
open Verify_utils

type t = (label * label) list
and trace = t

(* generate error trace *)
let gen : pgm -> label -> Z3.Model.model -> trace
= fun (a,b) last_t model ->

  (* find a state of tid at time t *)
  let find_state (tid,t) last_l : int =
    let rec find l =
      if l > last_l then assert false
      else if Solver.is_true model (v3 (tid,l,t)) then l
      else find (l+1)
    in
    find 0
  in

  (* the last label of each state *)
  let a_last, b_last = (get_last_state a, get_last_state b) in

  (* generate a state pair for each time t *)
  let rec loop t acc : (label * label) list =
    if t > last_t then List.rev acc
    else
      let a_l = find_state (get_tid a, t) a_last in
      let b_l = find_state (get_tid b, t) b_last in
      loop (t+1) ((a_l,b_l)::acc)
  in

  loop 0 []

let to_string : trace -> string
= fun trace ->
  trace
  |> List.map (fun (l_a,l_b) -> "(" ^ string_of_int l_a ^ "," ^ string_of_int l_b ^ ")")
  |> String.concat " -> "
