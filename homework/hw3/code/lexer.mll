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
            ("assert", ASSERT);
            ("fun", FUN);
            ("assert", ASSERT);
            ("return", RETURN);
            ("returns", RETURNS);
            ("int", INT);
            ("bool", BOOL);
           ]
}

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start = parse
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
  | "@pre" {PRE}
  | "@post" {POST}
  | "@L" {LINV}
  | "forall" {FORALL} | "exists" {EXISTS}
  | "len" {LEN} | "sorted" {SORTED} | "partitioned" {PARTITIONED}
  | "." {DOT}
  | "->" {RARROW}
  | "<->" {BIARROW}
  | "?" {HOLE}
  | number {NUM (int_of_string (Lexing.lexeme lexbuf))}
  | id {let id = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_tbl id
        with _ -> ID id}
  | eof {EOF}
  | _ { print_endline (Lexing.lexeme  lexbuf); raise LexicalError }

and comment = parse
  | "(*" {comment_depth := !comment_depth+1; comment lexbuf}
  | "*)" {comment_depth := !comment_depth-1;
          if !comment_depth > 0 then comment lexbuf }
  | eof {raise Eof}
  | _   {comment lexbuf}
