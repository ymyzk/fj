open Printf
open Typing

let print_error file lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let line = start.Lexing.pos_lnum in
  eprintf "%s:%d: %s\n" file line message

(* 1つのファイルを読み込んでパース処理を行い, Type checker にかける *)
let read_from_file file =
  print_endline ("Reading from " ^ file);
  let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    try
      let classes = Parser.toplevel Lexer.main lexbuf in
        print_endline "Running type check...";
        check classes;
        print_endline "Completed"
    with e ->
      begin match e with
        Failure message ->
          print_endline ("Failure: " ^ message)
      | Lexer.Lexer_error ->
          let token = Lexing.lexeme lexbuf in
          let message = sprintf "lexer error: unexpected token '%s'" token in
          print_error file lexbuf message
      | Parsing.Parse_error | Parser.Error -> (* ocamlyacc & Menhir *)
          let token = Lexing.lexeme lexbuf in
          let message = sprintf "parser error: unexpected token '%s'" token in
          print_error file lexbuf message
      | _ ->
        close_in_noerr ic;
        print_endline "unknown error";
        raise e
      end

let _ =
  let files = ref [] in
  Arg.parse
    []
    (fun file -> files := file :: !files)
    "FJ type checker";
  (* コマンドライン引数の順番とリストの順番が逆になっているので rev *)
  List.iter (fun f -> read_from_file f) (List.rev !files)
