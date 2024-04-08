open Util

let x = Var ("x", EType Int)
let y = Var ("y", EType Int)

let mk_add x y = BinOp (Add, x, y, EType Int)
let mk_mul x y = BinOp (Mul, x, y, EType Int)

let f1 = BinRel(Gt, x, Int 2)
let f2 = BinRel(Lt, y, Int 10)
let f3 = BinRel(Eq, mk_add x (mk_mul (Int 2) y), Int 7)

let final = And (f1,And (f2,f3))

let _ = check_sat_print final
