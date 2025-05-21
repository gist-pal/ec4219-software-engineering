type t = thread * thread

and pgm = t
and thread = tid * lcmd list
and lcmd = label * cmd
and label = int
and tid = string
and var = string

and cmd =
  | MayGoto of label
  | If of var * label * label
  | Set of var * bool * label
  | Critical of label

let get_tid (tid,lcmds) = tid
let get_lcmds (tid,lcmds) = lcmds

let get_last_state thread =
  List.length (get_lcmds thread) - 1

let collect_vars : pgm -> var BatSet.t
= fun (a,b) ->
  let collect' thread : var BatSet.t =
    List.fold_left (fun acc (_,cmd) ->
      match cmd with
      | MayGoto _ | Critical _ -> acc
      | If (x,_,_) | Set (x,_,_) -> BatSet.add x acc
    ) BatSet.empty (get_lcmds thread)
  in
  BatSet.union (collect' a) (collect' b)
