{
open Lexing

exception Lexer_error of string

let reservedWords = [
  (* Keywords *)
  ("class", Parser.CLASS);
  ("extends", Parser.EXTENDS);
  ("new", Parser.NEW);
  ("return", Parser.RETURN);
  ("super", Parser.SUPER);
  ("this", Parser.THIS);
]

let increment_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
  Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
  Lexing.pos_bol = pos.Lexing.pos_cnum;
}
}

rule main = parse
    [' ' '\009' '\012']+     { main lexbuf }
  | '\n' { increment_linenum lexbuf; main lexbuf }
  | "/*" { comments lexbuf }
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
      Not_found -> Parser.ID id
  }
  | eof { Parser.EOF }
  | _ {
    let token = Lexing.lexeme lexbuf in
    let message = Printf.sprintf "unexpected token '%s'" token in
    raise (Lexer_error message)
  }
and comments = parse
    "*/" { main lexbuf }
  | "\n" { increment_linenum lexbuf; comments lexbuf }
  | eof { raise (Lexer_error "unclosed comment") }
  | _ { comments lexbuf }
