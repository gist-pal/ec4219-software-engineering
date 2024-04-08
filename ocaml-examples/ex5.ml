let rec insert a l = (* assume increasing order *)
  match l with
  | [] -> [a]
  | hd::tl ->
    if a <= hd then a::hd::tl
    else hd::(insert a tl)
