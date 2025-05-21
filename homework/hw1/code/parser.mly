%{
  open Pgm
%}

%token <int> NUM
%token <string> ID
%token <bool> BOOL
%token TRUE FALSE
%token LPAREN RPAREN MINUS
%token AND IF THEN GOTO ELSE EQ
%token MAY COMMA
%token CRITICAL
%token THREAD_A THREAD_B
%token EOF

%start program
%type <Pgm.t> program

%%

program:
  MINUS THREAD_A MINUS thread
  MINUS THREAD_B MINUS thread EOF
  {(("A",$4),("B",$8))}

thread: lcmd_lst { $1 }

lcmd_lst:
  | lcmd lcmd_lst {$1::$2}
  | { [] }

lcmd: LPAREN NUM COMMA cmd RPAREN { ($2,$4)}

cmd:
  | MAY NUM {MayGoto $2}
  | IF ID THEN GOTO NUM ELSE GOTO NUM { If ($2,$5,$8) }
  | ID EQ BOOL AND GOTO NUM { Set ($1,$3,$6) }
  | CRITICAL AND GOTO NUM  { Critical $4 }

%%
