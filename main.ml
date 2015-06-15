open Typing

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

let _ =
  let files = ref [] in
  Arg.parse
    []
    (fun file -> files := file :: !files)
    "FJ type checker";
  (* コマンドライン引数の順番とリストの順番が逆になっているので rev *)
  List.iter (fun f -> read_from_file f) (List.rev !files)
