open Vocab
open Lang
open Formula
open Solver
open Guess
open Verify

module F = Formula
module L = Lang

let conjoin : F.formula BatSet.t -> F.formula
= fun set ->
  BatSet.fold (fun f acc ->
    if acc = F.True then f else F.And (acc, f)
  ) set F.True

(* formula annotated with related atomic candidate *)
type formula' = F.formula * acand
and acand = hid * F.formula (* refutation target *)

(* vcs1: processing vcs for validating each atomic candidate *)
(* vcs2: completed vcs for validating each atomic candidate *)
let rec pregen' : InvMap.t -> L.var * F.formula ->
                  formula' list * formula' list * cmd ->
                  formula' list * formula' list
= fun imap (rv,post) (vcs1,vcs2,cmd) ->
  match cmd with
  | While (Hole hid,e,c) -> raise NotImplemented (* TODO *)
  | While _ -> assert false (* assume all loops are not annotated *)
  | _ -> raise NotImplemented (* TODO *)

let verify_candidates : InvMap.t -> F.formula -> (L.var * F.formula) -> cmd -> acand list
= fun imap pre (rv,post) cmd ->
  let vcs =
    let (vcs1,vcs2) = pregen' imap (rv,post) ([],[],cmd) in
    let vcs1' = List.map (fun (g,acand) -> (Imply (pre,g),acand)) vcs1 in
    vcs1'@vcs2 in
  vcs
  |> List.filter (fun (g,acand) -> not (Solver.check_validity g))
  |> List.map snd

let rec refine : acand list -> InvMap.t -> InvMap.t
= fun refuted_lst imap ->
  match refuted_lst with
  | [] -> imap
  | (hid,acand)::tl ->
    print_endline ("- refuted atom: " ^ to_string_formula acand ^ " @ H#" ^ string_of_int hid);
    let imap' = InvMap.add hid (BatSet.diff (InvMap.find hid imap) (BatSet.singleton acand)) imap in
    refine tl imap'

let iter = ref 0

let rec loop : InvMap.t -> pgm -> InvMap.t
= fun imap ((pre,post,fid,iparams,rparam,cmd) as pgm) ->
  iter := !iter + 1;
  print_endline ("[INFO] iter: " ^ string_of_int !iter);
  let rv = snd rparam in
  let vcs_refuted = verify_candidates imap pre (rv,post) cmd in
  if List.length vcs_refuted = 0 then imap
  else
    let imap' = refine vcs_refuted imap in
    loop imap' pgm

let synthesize = loop

let gen_vc imap rv (pre,cmd,post) : F.formula * F.formula list =
  let (vc_executed,vcs_a1,vcs_a2) = Verify.pregen (rv,post) (post,[],[],cmd) in
  let vc_correct = F.And (F.Imply (pre, vc_executed), Verify.check_inv (rv,post) (post,cmd)) in
  let vcs_a = (List.map (fun g -> F.Imply (pre,g)) vcs_a1) @ vcs_a2 in
  (vc_correct, vcs_a)

(* fill in the hole *)
let fill : InvMap.t -> pgm -> pgm * cmd
= fun imap ((pre,post,fid,iparams,rparam,cmd) as pgm) ->
  let rec fill' imap cmd =
    match cmd with
    | While (Hole hid,e,c) ->
      let inv = conjoin (InvMap.find hid imap) in
      While (inv, e, fill' imap c)
    | While (inv,e,c) -> While (inv, e, fill' imap c)
    | Seq (c1,c2) -> Seq (fill' imap c1, fill' imap c2)
    | If (e,c1,c2) -> If (e, fill' imap c1, fill' imap c2)
    | _ -> cmd in
  let update_cmd (a,b,c,d,e,_) cmd = (a,b,c,d,e,cmd) in
  let cmd' = fill' imap cmd in
  (update_cmd pgm cmd', cmd')

let run : pgm -> bool * (pgm * int) option
= fun ((pre,post,fid,iparams,rparam,cmd) as pgm) ->
  let imap0 = Guess.run pgm in
  let imap = synthesize imap0 pgm in
  let _ = print_endline "[INFO] inductive invariants found!" in
  let (pgm',cmd') = fill imap pgm in
  let (vc_correct, vcs_assert) = gen_vc imap (snd rparam) (F.And (pre, len_non_neg), cmd', post) in
  if not (Solver.check_validity vc_correct) then
    (print_endline ("[INFO] fail to prove partial correctness");
    (false, Some (pgm', List.length (List.filter Solver.check_validity vcs_assert))))
  else
    let proven = List.length (List.filter Solver.check_validity vcs_assert) in
    (true, Some (pgm', proven))
