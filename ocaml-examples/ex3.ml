let rec nth l n =
  match l with
  | [] -> raise (Failure "list is too short")
  | hd::tl -> if n = 0 then hd else nth tl (n-1)

let tc1 = (([1;2;3], 2), 3)

let run f ((l,n),out) = (f l n = out)

let _ = print_endline (string_of_bool (run nth tc1))
let _ = print_endline (string_of_bool (run nth tc1))
