open Pgm
open Options

let main () =
  let usageMsg = "./main.native -input filename" in
  let _ = Arg.parse options (fun s->()) usageMsg in
  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let pgm = Parser.program Lexer.start lexbuf in

  let t0 = Sys.time () in
  let res = Verify.run pgm in
  let t1 = Sys.time () in

  begin
    print_endline "===== Verification Result ======";
    match res with
    | None -> print_endline "Safe"
    | Some (bound,model) -> (
      Printf.printf "Unsafe at bound %d\n" bound;
      let error_trace = Trace.gen pgm bound model in
      print_endline ("Error trace: " ^ Trace.to_string error_trace)
      )
  end;
  Printf.eprintf "Time: %.3f seconds\n%!" (t1 -. t0)

let _ = main ()
