open Parser
open Lexer
open Lexing
open Printf

(*
let parse_with_error lexbuf =
 try Parser.top Lexer.read lexbuf with
 | SyntaxError msg -> prerr_string (msg ^ ": ");
                      print_position lexbuf;
                      exit (-1)
 | Exp_par.Error ->   prerr_string "Parse error: ";
                      print_position lexbuf;
                      exit (-1)
*)

(*Functions to print the exception and the position - taken from Real World OCaml *)

let print_Position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "Line number :%d, Position: %d" 
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse_with_error lexbuf =
  try Parser.top Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_Position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_Position lexbuf;
    exit (-1)





(*let rec read_to_empty buf =
    let s = read_line () in
    if s = "" then buf
    else (Buffer.add_string buf s;
          Buffer.add_string buf "\n";
          read_to_empty buf)

let _ =
  read_to_empty (Buffer.create 1)
  |> Buffer.contents
  |> Lexing.from_string
  |> parse_with_error
  |> print_string;
    print_newline () *) 

let parseFile fileName =
  let channel = open_in fileName in
  Lexing.from_channel channel
  |> parse_with_error
  |> print_string;
  print_newline ();
  close_in channel
	   
let _ =
  if Array.length Sys.argv > 1
  then parseFile Sys.argv.(1)
  else (read_line ()
       |> Lexing.from_string
       |> parse_with_error
       |> print_string;
       print_newline ())


