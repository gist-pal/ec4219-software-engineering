let inputfile = ref ""
let max_bound = ref 30

let options =
  [
    ("-input", Arg.String (fun s -> inputfile := s), "inputfile containing your examples");
    ("-max_bound", Arg.Int (fun n -> max_bound := n), "maximum verification bound (default : 30)")
  ]
