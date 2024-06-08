open Vocab
open Lang
open AbsDom
open AbsDom.Itv

let len_arr = ("len", DArray Int)

let rec eval_c : cmd -> AbsMem.t * int -> AbsMem.t * int
= fun cmd (m,proven) -> raise NotImplemented (* TODO *)

let init_iparam m (typ,x) =
  match typ with
  | ETyp Int -> AbsMem.add x (Itv.top, AbsBool.Bot) m
  | ETyp Bool -> AbsMem.add x (Itv.Bot, AbsBool.Top) m
  | DArray Int
  | FArray (Int,_) ->
    m
    |> AbsMem.add x (Itv.top, AbsBool.Bot)
    |> AbsMem.add len_arr (Itv.top, AbsBool.Bot)
  | _ -> failwith "init_iparam: boolean arrays are out of scope"

let run : pgm -> int
= fun (pre,post,fid,iparams,rparam,cmd) ->
  let m0 = List.fold_left init_iparam AbsMem.empty iparams in
  print_endline "[INFO] initial memory state";
  AbsMem.print m0;
  snd (eval_c cmd (m0,0))
