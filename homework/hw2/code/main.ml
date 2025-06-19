open Lang
open Options

let main () =
  let usageMsg = "./main.native -input filename" in
  Arg.parse options (fun s->()) usageMsg;

  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let pgm = Parser.program Lexer.start lexbuf in
  let pgm = Preprocess.run pgm in

  print_endline "=== Program ===";
  print_endline (to_string_pgm pgm);

  let t0 = Sys.time () in
  let (correct, assert_proven) = Verify.run pgm in
  let t1 = Sys.time () in

  print_endline "=== Verification Result ===";
  print_endline (string_of_bool correct ^ ", " ^ string_of_int assert_proven);
  print_endline ("Time : " ^ string_of_float (t1 -. t0) ^ "seconds")

let _ = main ()
