{

open Parser 
open Lexing
exception SyntaxError of string



(*Function that holds the current location and updates the line number - taken  from Real World OCaml   *)
let curr_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}



let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let float = int '.' ['0'-'9'] ['0'-'9']*


rule read = 
   parse
   | white { read lexbuf } 
   | newline { read lexbuf } 
   | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } *)
   | "print" { PRINT }
   | ';' { SEMICOLON }
   | '"' { readString (Buffer.create 16) lexbuf }
   | '+' { PLUS }
   | '*' { TIMES }
   | '-' { MINUS }
   | '/' { DIV }
   | "mod" { MOD }
   | '(' { LEFTBR }
   | ')' { RIGHTBR}
   | _ { raise (SyntaxError ("Unexpected character: " ^ (Lexing.lexeme lexbuf) ^ "\n")) }
   | eof { EOF }
and readString buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; readString buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; readString buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; readString buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; readString buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; readString buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; readString buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; readString buf lexbuf }
  | [^ '"' '\\']+
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      readString buf lexbuf
    }
