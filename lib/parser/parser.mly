%{
open Ast
%}

(*TYPES*)
%token I32 BOOL TBOX

(*VALUES*)

%token <int> INT
%token <string> IDENT
%token TRUE FALSE

(*KEYWORDS*)
%token LET MUT MOVE COPY BOX ENDLIFETIME
%token AND ANDAND OROR NOT STAR
%token PLUS MINUS SLASH PERCENT
%token LT GT LE GE EQ NE
%token ASSIGN COLON SEMI
%token LBRACE RBRACE
%token EOF

%start <statement list> program

%%

program:
  | stmts = list(statement); EOF { stmts }

statement:
  | LET; mut = boption(MUT); id = IDENT; COLON; typ = type_expr; 
    ASSIGN; e = expr; SEMI
    { Let(mut, id, typ, e, { loc_start = $startpos; loc_end = $endpos }) }
  | e = expr; SEMI
    { Expr(e, { loc_start = e.expr_loc.loc_start; loc_end = $endpos }) }
  | LBRACE; stmts = list(statement); RBRACE
    { Block(stmts, { loc_start = $startpos; loc_end = $endpos }) }

type_expr:
  | I32            { TInt }
  | BOOL           { TBool }
  | AND; t = type_expr 
    { TRef(t) }
  | AND; MUT; t = type_expr 
    { TRefMut(t) }
  | TBOX; LT; t = type_expr; GT   
    { TBox(t) }

expr:
  | i = INT
    { { expr_desc = Int(i); 
        expr_type = TInt;
        expr_loc = { loc_start = $startpos; loc_end = $endpos } } }
  | TRUE 
    { { expr_desc = Bool(true);
        expr_type = TBool;
        expr_loc = { loc_start = $startpos; loc_end = $endpos } } }
  | FALSE
    { { expr_desc = Bool(false);
        expr_type = TBool;
        expr_loc = { loc_start = $startpos; loc_end = $endpos } } }
  | id = IDENT
    { { expr_desc = Var(id);
        expr_type = TPlaceholder;
        expr_loc = { loc_start = $startpos; loc_end = $endpos } } }
  | op = unop; e = expr
    { { expr_desc = Unop(op, e);
        expr_type = TPlaceholder;
        expr_loc = { loc_start = $startpos; loc_end = $endpos } } }
  | e1 = expr; op = binop; e2 = expr
    { { expr_desc = Binop(op, e1, e2);
        expr_type = TPlaceholder; 
        expr_loc = { loc_start = e1.expr_loc.loc_start; loc_end = e2.expr_loc.loc_end } } } 

%inline unop:
  | MINUS       { Neg }
  | NOT         { Not }
  | AND         { Ref }
  | AND; MUT    { RefMut }
  | STAR        { Deref }
  | MOVE        { Move }
  | BOX         { Box }
  | COPY        { Copy }
  | ENDLIFETIME { EndLifetime }

%inline binop:
  | PLUS      { Add }
  | MINUS     { Sub }
  | STAR      { Mul }
  | SLASH     { Div }
  | PERCENT   { Mod }
  | LT        { Lt }
  | GT        { Gt }
  | LE        { Le }
  | GE        { Ge }
  | EQ        { Eq }
  | NE        { Ne }
  | ANDAND    { And }
  | OROR      { Or }
