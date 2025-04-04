open Verifier.Typecheck
open Verifier.Error

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Lexer.LexError msg ->
      close_in ic;
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      close_in ic;
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at %s:%d:%d\n" filename pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
      exit 1

let process_file filename =
  Printf.printf "Processing file: %s\n" filename;
  let program = parse_file filename in
  try
    type_check program;
    Printf.printf "✅ Type checking successful!\n"
  with Error.TypeError (err, loc) ->
    Error.report_error err loc;
    exit 1

let print_usage () =
  Printf.printf "Usage: %s <filename>\n" Sys.argv.(0);
  Printf.printf "Options:\n";
  Printf.printf "  --help, -h     Print this help message\n"

let () =
  match Array.length Sys.argv with
  | 1 ->
      print_usage ();
      exit 0
  | _ ->
      if Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h" then print_usage ()
      else process_file Sys.argv.(1)
