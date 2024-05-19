%{
  open Lang
  open Formula
  module L = Lang
  module F = Formula
%}

(***************)
(*** Formula ***)
(***************)

%token FORALL EXISTS RARROW BIARROW
%token DOT
%token PRE POST LINV
%token <int> NUM
%token <string> ID
%token TRUE FALSE
%token SORTED PARTITIONED

%token LEN
%token INT BOOL RETURNS
%token PLUS MINUS MUL EQ EQEQ NEQ
%token LE LT GE GT NOT OR AND
%token FUN RARROW IF WHILE AT RETURN SKIP COMMA ELSE ASSERT SEMICOLON
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN EOF

%left SEMICOLON
%right DOT FORALL EXISTS
%right BIARROW
%right RARROW
%right OR
%right AND
%nonassoc LT LE GT GE EQEQ NEQ
%left PLUS MINUS
%left MUL
%right NOT

%start program
%type <Lang.pgm> program

%%

program:
  PRE LBRACE formula RBRACE
  POST LBRACE formula RBRACE
  ID LPAREN params RPAREN RETURNS LPAREN param RPAREN
  LBRACE cmd RBRACE EOF
  {($3,$7,$9,$11,$15,$18)}

params:
  | param COMMA params {$1::$3}
  | param {[$1]}

param:
  | typ ID { ($1,($2,$1)) }

typ:
  | INT { ETyp Int }
  | BOOL { ETyp Bool }
  | INT LBRACKET RBRACKET { DArray Int }
  | BOOL LBRACKET RBRACKET { DArray Bool }
  | INT LBRACKET NUM RBRACKET { FArray (Int,$3) }
  | BOOL LBRACKET NUM RBRACKET { FArray (Bool,$3) }

formula:
  | TRUE {F.True}
  | FALSE {F.False}
  | SORTED LPAREN term COMMA term COMMA term RPAREN
    { Sorted ($3, $5, $7) }
  | PARTITIONED LPAREN term COMMA term COMMA term COMMA term COMMA term RPAREN
    { Partitioned ($3,$5,$7,$9,$11) }
  | ID { PVar $1 }
  | LPAREN formula RPAREN { $2 }
  | term EQ term {F.BinRel (F.Eq,$1,$3)}
  | term NEQ term {F.BinRel (F.Neq,$1,$3)}
  | term LT term {F.BinRel (F.Lt,$1,$3)}
  | term LE term {F.BinRel (F.Leq,$1,$3)}
  | term GT term {F.BinRel (F.Gt,$1,$3)}
  | term GE term {F.BinRel (F.Geq,$1,$3)}
  | NOT formula {F.Not $2}
  | formula OR formula {F.Or ($1,$3)}
  | formula AND formula {F.And ($1,$3)}
  | formula RARROW formula {Imply ($1,$3)}
  | formula BIARROW formula {Iff ($1,$3)}
  | FORALL bvars DOT formula {Forall ($2,$4)}
  | EXISTS bvars DOT formula {Exists ($2,$4)}

bvars:
  | bvar COMMA bvars { $1::$3 }
  | bvar { [$1] }

bvar:
  | ID { if "i" <= $1 then ($1, ESort Int) else ($1, Array (ESort Int, ESort Int)) }

term:
  | MINUS NUM {Int ($2* (-1))}
  | NUM {Int $1}
  | LEN LPAREN ID RPAREN {Len ($3, NullSort)}
  | ID {Var ($1, NullSort)}
  | ID LBRACKET term RBRACKET {F.Read (F.Var ($1, ESort Int),$3)}
  | term PLUS term {F.BinOp (Add,$1,$3, ESort Int)}
  | term MINUS term {F.BinOp (Sub,$1,$3, ESort Int)}
  | term MUL term {F.BinOp (Mul,$1,$3, ESort Int)}

cmd:
  | atom_cmd SEMICOLON { $1 }
  | atom_cmd SEMICOLON cmd {L.Seq ($1, $3)}
  | compound_cmd { $1 }
  | compound_cmd cmd {L.Seq ($1,$2)}

atom_cmd:
  | typ ID { L.Decl ($1, ($2,$1)) }
  | lv EQ exp { L.Assign ($1, $3) }
  | ASSERT LPAREN exp RPAREN {L.Assert ($3)}
  | RETURN exp {L.Return $2}

compound_cmd:
  | IF LPAREN exp RPAREN LBRACE cmd RBRACE ELSE LBRACE cmd RBRACE {L.If ($3,$6,$10)}
  | IF LPAREN exp RPAREN LBRACE cmd RBRACE {L.If ($3,$6,L.Skip)}
  | WHILE
      LINV LBRACE formula RBRACE
      LPAREN exp RPAREN
      LBRACE cmd RBRACE {L.While ($4,$7,$10)}

exp:
  | MINUS NUM {L.Int ($2* (-1))}
  | NUM {L.Int $1}
  | LEN LPAREN ID RPAREN {Len ($3,NullTyp)}
  | lv {L.Lv $1}
  | exp PLUS exp {L.Plus ($1,$3)}
  | exp MINUS exp {L.Minus ($1,$3)}
  | exp MUL exp {L.Mul ($1,$3)}
  | TRUE {L.True}
  | FALSE {L.False}
  | exp EQEQ exp {L.Eq ($1,$3)}
  | exp NEQ exp {L.Neq ($1,$3)}
  | exp LT exp {L.Lt ($1,$3)}
  | exp LE exp {L.Leq ($1,$3)}
  | exp GT exp {L.Gt ($1,$3)}
  | exp GE exp {L.Geq ($1,$3)}
  | NOT exp {L.Not $2}
  | exp OR exp {L.Or ($1,$3)}
  | exp AND exp {L.And ($1,$3)}

lv:
  | ID {L.Var ($1,NullTyp)}
  | ID LBRACKET exp RBRACKET {L.Arr (($1,NullTyp),$3)}

%%
