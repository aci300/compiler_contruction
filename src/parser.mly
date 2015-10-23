
(* The parser for the compiler *)

%{

 type oType = Bool of bool
		   | Int of int
		   | Float of float
		   | String of string

    let stOfType = function
      | Bool b    -> string_of_bool b
      | Int i     -> string_of_int i
      | Float f   -> string_of_float f
      | String s  -> s


%}


%token <int> INT
%token <float> FLOAT 
%token <string> STRING
%token <string> VAR
%token <bool> BOOL
%token PRINT
%token FUNC
%token EQUALS
%token AND
%token OR
%token SEMICOLON
%token LEFTBR
%token RIGHTBR
%token MINUS
%token PLUS
%token TIMES
%token DIV
%token MOD
%token EOF
%left AND
%left OR
%left PLUS
%left MINUS
%left TIMES
%left DIV
%left MOD
%start <string> top
%%

top : 
    | el = exp; option(SEMICOLON); EOF { "int : " ^ (string_of_int el) }
   | s = string; option(SEMICOLON); EOF { "A string. " ^ s }
   | FUNC; name = VAR; l = list(v = VAR { v }); EQUALS { "Start of a function called " ^ name }
  | p = print; option(SEMICOLON); EOF { "" }
  | p = print; SEMICOLON; s = top { s }
 
					     
  | SEMICOLON; s = top { s } 
  | SEMICOLON; EOF { "" }

string:
  | s = STRING { s }

print:
  | PRINT; s = STRING { print_string s }
  | PRINT; e = exp { print_string (string_of_int e) }



exp: 
   | i = INT { i} 
   | e = exp; PLUS; f= exp { e + f}
   | e = exp; MINUS; f= exp { e - f}
   | e = exp; TIMES; f= exp { e * f}
   | e = exp; DIV; f= exp { e / f}
   | e = exp; MOD; f= exp { e mod f}


