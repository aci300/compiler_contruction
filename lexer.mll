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
   | ';' { SEMICOLON }
   | '+' { PLUS }
   | '*' { TIMES }
   | '-' { MINUS }
   | '/' { DIV }
   | "mod" { MOD }
   | '(' { LEFTBR }
   | ')' { RIGHTBR}
   | _ { raise (SyntaxError ("Unexpected character: " ^ (Lexing.lexeme lexbuf) ^ "\n")) }
   | eof { EOF }
