open Lang
open Vocab
open Options

let main () =
  let usageMsg = "./main.native -input filename" in
  let _ = Arg.parse options (fun s->()) usageMsg in
  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let pgm = Parser.program Lexer.start lexbuf in
  let pgm = Preprocess.run pgm in
  let _ = print_endline "========== Program ==========" in
  let _ = print_endline (to_string_pgm pgm) in
  let t0 = Sys.time () in
  let _ =
  let _ = print_endline "========== Processing ==========" in
    match Verify2.run pgm with
    | (correct, Some (pgm',proven)) ->
      begin
        print_endline "========== Annotated Program ==========";
        print_endline (to_string_pgm pgm');
        print_endline "========== Verificaiton Result ==========";
        print_endline (string_of_bool correct ^ ", " ^ string_of_int proven)
      end
    | (b, None) -> print_endline "fail" in
  let t1 = Sys.time () in
  print_endline ("Time : " ^ string_of_float (t1 -. t0) ^ "seconds")

let _ = main ()
