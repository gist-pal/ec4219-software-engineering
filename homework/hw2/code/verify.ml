open Lang
open Term
open Utils
open Solver

exception NotImplemented

type assert_vc = line * T.formula

let gen_vc : pgm -> T.formula * assert_vc list
= fun (pre,post,fid,iparams,rparam,cmd) ->
  raise NotImplemented (* TODO *)

let check_assert : assert_vc list -> int
= fun assert_vcs ->
  let groups = BatList.group (fun (l1,_) (l2,_) -> Stdlib.compare l1 l2) assert_vcs in
  BatList.fold_lefti (fun acc i group ->
    let line = fst (List.hd group) in
    let safe = List.for_all (fun (l,g) -> Solver.check_valid g) group in
    Printf.eprintf "[%d] line %d : %s\n%!"
      (i+1) line (if safe then "safe" else "potentially unsafe");
    acc + (if safe then 1 else 0)
  ) 0 groups

let run : pgm -> bool * int
= fun ((pre,post,fid,iparams,rparam,cmd) as pgm) ->
  let (f_spec, assert_vcs) = gen_vc pgm in
  if not (Solver.check_valid f_spec) then (false,0)
  else
    let proven_num = check_assert assert_vcs in
    (true, proven_num)
