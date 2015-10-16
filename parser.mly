(* The parser for the compiler *)


%token <int> INT
%token <float> FLOAT 
%token <string> STRING
%token <bool> BOOL
%token FUNC
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
%start <int list> top
%%

top : 
   | el = separated_list(SEMICOLON, exp ); EOF { el}


exp: 
   | i = INT { i} 
   | e = exp; PLUS; f= exp { e + f}
   | e = exp; MINUS; f= exp { e - f}
   | e = exp; TIMES; f= exp { e * f}
   | e = exp; DIV; f= exp { e / f}
   | e = exp; MOD; f= exp { e mod f}
