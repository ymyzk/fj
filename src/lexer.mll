{
open Lexing

let reservedWords = [
  (* Keywords *)
  ("class", Parser.CLASS);
  ("extends", Parser.EXTENDS);
  ("new", Parser.NEW);
  ("return", Parser.RETURN);
  ("super", Parser.SUPER);
  ("this", Parser.THIS);
]
}

rule main = parse
  [' ' '\009' '\012' '\n']+     { main lexbuf }
| "," { Parser.COMMA }
| "." { Parser.PERIOD }
| "=" { Parser.EQ }
| "{" { Parser.LBRACE }
| "(" { Parser.LPAREN }
| "}" { Parser.RBRACE }
| ")" { Parser.RPAREN }
| ";" { Parser.SEMICOLON }
| ['a'-'z' 'A'-'Z']+ {
  let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with
(*       _ -> print_endline (Printf.sprintf "ID: %s l: %d" id (Lexing.lexeme_start lexbuf)); *)
       _ -> print_endline (Printf.sprintf "ID: %s l: %d" id ((Lexing.lexeme_start_p lexbuf).pos_lnum)); 
      Parser.ID id
  }
| eof { Parser.EOF }
