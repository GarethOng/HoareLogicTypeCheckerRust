type location = { loc_start : Lexing.position; loc_end : Lexing.position }

let no_location = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos }

type typ =
  | TInt (* i32 *)
  | TBool (* bool *)
  | TUnit (* () *)
  | TPlaceholder (* () *)
  | TRef of typ (* &T *)
  | TRefMut of typ (* &mutT *)
  | TBox of typ (* □T *)

type binop =
  (* Arithmetic *)
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  (* Comparison *)
  | Lt
  | Gt
  | Le
  | Ge
  (* Logical *)
  | Eq
  | Ne
  | And
  | Or

type unop =
  | Neg (* Arithmetic negation *)
  | Not (* Logical not *)
  | Ref (* & *)
  | RefMut (* &mut *)
  | Deref (* * *)
  | Move (* move *)
  | Box (* box *)
  | Copy (* copy *)
  | EndLifetime (* End lifetime of L-Value*)

type expr = { expr_desc : expr_desc; expr_type : typ; expr_loc : location }

and expr_desc =
  | Int of int
  | Bool of bool
  | Unit
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr

type statement =
  | Let of bool * string * typ * expr * location (* bool is for mutability *)
  | Expr of expr * location
  | Block of statement list * location
