open Vocab
open Lang
open Formula
open Solver
open Guess
open Verify

let run : pgm -> bool * (pgm * int) option
= fun ((pre,post,fid,iparams,rparam,cmd) as pgm) ->
  let _ = Guess.run pgm in
  raise NotImplemented (* TODO *)
