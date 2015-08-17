open Printf
open Typing

let print_info file message =
  eprintf "%s: %s\n" file message

let print_error lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let file = start.Lexing.pos_fname in
  let line = start.Lexing.pos_lnum in
  eprintf "%s:%d: %s\n" file line message

let print_error2 start message =
  let file = start.Lexing.pos_fname in
  let line = start.Lexing.pos_lnum in
  eprintf "%s:%d: %s\n" file line message

(* 1つのファイルを読み込んでパース処理を行い, Type checker にかける *)
let read_from_file file =
  print_info file "reading";
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- {
    lexbuf.Lexing.lex_curr_p with
    Lexing.pos_fname = file;
  };
  try
    let classes = Parser.toplevel Lexer.main lexbuf in
    print_info file "running type check";
    check classes;
    print_info file "completed"
  with e ->
    begin match e with
      Failure message ->
        let message = sprintf "failure: %s" message in
        print_info file message;
        exit 1
    | Lexer.Lexer_error message ->
        let message = sprintf "lexer error: %s" message in
        print_error lexbuf message;
        exit 1
    | Parser.Error -> (* Menhir *)
        let token = Lexing.lexeme lexbuf in
        let message = sprintf "parser error: unexpected token '%s'" token in
        print_error lexbuf message;
        exit 1
    | Type_error(position, message) ->
        let message = sprintf "type error: %s" message in
        print_error2 position message;
        exit 1
    | _ ->
        close_in_noerr ic;
        print_info file "unknown error";
        raise e
    end

let _ =
  let files = ref [] in
  Arg.parse
    []
    (fun file -> files := file :: !files)
    "Featherweight Java type checker";
  (* コマンドライン引数の順番とリストの順番が逆になっているので rev *)
  List.iter read_from_file (List.rev !files)
