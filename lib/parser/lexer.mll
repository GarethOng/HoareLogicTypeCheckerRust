{
open Parser
exception LexError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha|digit|'_')*
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace    { token lexbuf }
  | digit+ as n   { INT(int_of_string n) }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "let"         { LET }
  | "mut"         { MUT }
  | "i32"         { I32 }
  | "bool"        { BOOL }
  | "&"           { AND }
  | "&&"          { ANDAND }
  | "||"          { OROR }
  | "!"           { NOT }
  | "*"           { STAR }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "/"           { SLASH }
  | "%"           { PERCENT }
  | "<"           { LT }
  | ">"           { GT }
  | "<="          { LE }
  | ">="          { GE }
  | "=="          { EQ }
  | "!="          { NE }
  | "="           { ASSIGN }
  | ":"           { COLON }
  | ";"           { SEMI }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | ident as id   { IDENT(id) }
  | eof           { EOF }
  | _             { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

