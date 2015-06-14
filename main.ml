open Typing
(*
open Eval
open Syntax
open Typing

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10)
        (Environment.extend "ii" (IntV 2)
          (Environment.extend "iii" (IntV 3)
            (Environment.extend "iv" (IntV 4)
              Environment.empty)))))

let initial_tyenv =
  Environment.extend "i" (tysc_of_ty TyInt)
    (Environment.extend "v" (tysc_of_ty TyInt)
      (Environment.extend "x" (tysc_of_ty TyInt)
        (Environment.extend "ii" (tysc_of_ty TyInt)
          (Environment.extend "iii" (tysc_of_ty TyInt)
            (Environment.extend "iv" (tysc_of_ty TyInt)
              Environment.empty)))))

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (newtyenv, ty) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
      Printf.printf "val %s : " id;
      pp_ty ty;
      print_string " = ";
      pp_val v;
      print_newline();
      read_eval_print newenv newtyenv
  with
    Error s ->
      Printf.printf "Eval Error: %s\n" s;
      read_eval_print env tyenv
  | TypeError s ->
      Printf.printf "Type Error: %s\n" s;
      read_eval_print env tyenv
  | Failure s ->
      Printf.printf "Lexer Error: %s\n" s;
      read_eval_print env tyenv
  | Parsing.Parse_error ->
      print_string "Parser Error:\n";
      read_eval_print env tyenv
  | _ ->
      print_string "Unexpected Error:\n";
      read_eval_print env tyenv

let _ = read_eval_print initial_env initial_tyenv
*)

let rec read_eval_print i =
  print_string ">>> ";
  flush stdout;
  (*
  try
    *)
    let classes = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    print_newline();
    check classes;
    read_eval_print 1
  (*
  with
    Error s ->
      Printf.printf "Eval Error: %s\n" s;
      read_eval_print ()
  | TypeError s ->
      Printf.printf "Type Error: %s\n" s;
      read_eval_print ()
  | Failure s ->
      Printf.printf "Lexer Error: %s\n" s;
      read_eval_print env tyenv
  | Parsing.Parse_error ->
      print_string "Parser Error:\n";
      read_eval_print env tyenv
    _ ->
      print_string "Unexpected Error:\n";
      read_eval_print 1
      *)

(*let _ = read_eval_print 1*)


let read_from_file file =
  print_endline ("Reading from " ^ file);
  let ic = open_in file in
    try
      let classes = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
        print_endline "Running type check...";
        check classes;
        print_endline "Completed"
    with e ->
      close_in_noerr ic;
      print_endline "Error";
      raise e

(* let _ = read_from_file "test.java" *)

let _ =
  let files = ref [] in
  Arg.parse
    []
    (fun file -> files := file :: !files)
    "FJ type checker";
  (* コマンドライン引数の順番とリストの順番が逆になっているので rev *)
  List.iter (fun f -> read_from_file f) (List.rev !files)
