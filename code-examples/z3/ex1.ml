open Util

let p = PVar "p"
let q = PVar "q"
let r = PVar "r"

let f1 = Imply (p,q)
let f2 = Iff (r, Not q)
let f3 = Or (Not p, r)

let final = And (f1,And (f2,f3))

let _ = check_sat_print final
