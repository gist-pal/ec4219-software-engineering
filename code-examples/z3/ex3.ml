open Util

let mk_read (t1,t2) = Read (t1,t2)
let mk_write (t1,t2,t3) = Write (t1,t2,t3)

let i = Var ("i", EType Int)
let e = Var ("e", EType Int)

let a = Var ("a", Array Int)

let pre = BinRel (Eq, mk_read (a, i), e)
let con =
  let j = Var ("@j", EType Int) in
  Forall ([("@j", EType Int)], BinRel (Eq, mk_read (mk_write (a,i,e), j),mk_read (a,j)))

let final = Imply (pre, con)

let _ = check_sat_print (Not final)
