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

  (*TYPE ANNOTATIONS *)
  | "i32"         { I32 }
  | "bool"        { BOOL }
  | "Box"         { TBOX }
  | ":"           { COLON }

  (* VALUES *)
  | digit+ as n   { INT(int_of_string n) }
  | "true"        { TRUE }
  | "false"       { FALSE }

  (* KEYWORDS *)
  | "let"         { LET }
  | "mut"         { MUT }
  | "move"        { MOVE }
  | "copy"        { COPY }
  | "box"         { BOX }

  (* UNOPS *)
  | "&"           { AND }
  | "!"           { NOT }

  (* BINOPS *)

  (* bool *)
  | "&&"          { ANDAND }
  | "||"          { OROR }

  (* i32 *)
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

  (* bool/i32 *)
  | "!="          { NE }
  | "="           { ASSIGN }
  | ";"           { SEMI }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | ident as id   { IDENT(id) }
  | eof           { EOF }
  | _             { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

