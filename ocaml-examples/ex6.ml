(* Do not change the template *)
(* For example, do not change the definition of the variants, the type of the function, etc. *)

type exp =
  | Int of int
  | Minus of exp * exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Div of exp * exp

let rec eval : exp -> int
= fun exp -> (* TODO *)
