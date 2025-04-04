{
open Parser
exception LexError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha|digit|'_')*
let whitespace = [' ' '\t']

rule token = parse
  | whitespace    { token lexbuf }
  | '\n'          { Lexing.new_line lexbuf; token lexbuf }
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

  (* UNOPS *)
  | "&"           { AND }
  | "!"           { NOT }
  | "*"           { STAR }
  | "move"        { MOVE }
  | "copy"        { COPY }
  | "box"         { BOX }
  | "endlifetime" { ENDLIFETIME }

  (* BINOPS *)

  (* bool *)
  | "&&"          { ANDAND }
  | "||"          { OROR }

  (* i32 *)
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

