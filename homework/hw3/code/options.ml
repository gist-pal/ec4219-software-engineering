let inputfile = ref ""
let itv = ref false

let options =
  [
    ("-input", (Arg.String (fun s -> inputfile := s)), "inputfile containing your examples");
  ]
