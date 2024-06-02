let inputfile = ref ""
let simple = ref false

let options = 
  [
    ("-input", (Arg.String (fun s -> inputfile := s)), "inputfile containing your examples");
    ("-simple", (Arg.Set simple), "simple")
  ]
