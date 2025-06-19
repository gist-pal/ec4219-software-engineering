{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword,tok) -> Hashtbl.add keyword_tbl keyword tok)
           [
            ("true", TRUE);
            ("false", FALSE);
            ("if", IF);
            ("else", ELSE);
            ("while", WHILE);
            ("return", RETURN);
            ("returns", RETURNS);
            ("int", INT);
            ("bool", BOOL);
           ]
}

let line_break = "\r\n" | "\n"
let blank = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start = parse
  | line_break { Lexing.new_line lexbuf; start lexbuf }
  | blank { start lexbuf }
  | "(*" { comment_depth :=1;
           comment lexbuf;
           start lexbuf }
  | "+" {PLUS} | "-" {MINUS} | "*" {MUL}
  | "=" {EQ} | "==" {EQEQ} | "!=" {NEQ}
  | "<" {LT} | "<=" {LE} | ">" {GT} | ">=" {GE}
  | "!" {NOT} | "&&" {AND} | "||" {OR}
  | "," {COMMA} | ";" {SEMICOLON}
  | "(" {LPAREN} | ")" {RPAREN}
  | "{" {LBRACE} | "}" {RBRACE}
  | "[" {LBRACKET} | "]" {RBRACKET}
  | "@pre" {PRE} | "@post" {POST} | "@L" {LINV}
  | "forall" {FORALL} | "exists" {EXISTS} | "." {DOT}
  | "len" {LEN} | "sorted" {SORTED} | "partitioned" {PARTITIONED}
  | "->" {RARROW} | "<->" {BIARROW}
  | "true" {TRUE} | "false" {FALSE}
  | "if" {IF} | "else" {ELSE} | "while" {WHILE}
  | "return" {RETURN} | "returns" {RETURNS}
  | "int" {INT} | "bool" {BOOL}
  | "assert" { let pos = lexbuf.Lexing.lex_start_p in
               ASSERT pos.pos_lnum }
  | number {NUM (int_of_string (Lexing.lexeme lexbuf))}
  | id {ID (Lexing.lexeme lexbuf)}
  | eof {EOF}
  | _ { print_endline (Lexing.lexeme lexbuf); raise LexicalError }

and comment = parse
  | "(*" {comment_depth := !comment_depth+1; comment lexbuf}
  | "*)" {comment_depth := !comment_depth-1;
          if !comment_depth > 0 then comment lexbuf }
  | eof {raise Eof}
  | _   {comment lexbuf}
