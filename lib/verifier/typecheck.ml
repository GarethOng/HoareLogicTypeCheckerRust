open Ast
open Environment
open Error

type propagated_lifetime = Environment.lifetime

let rec compatible_types (expected : Ast.typ) (actual : Ast.typ) : bool =
  match (expected, actual) with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> true
  | TRef t1, TRef t2 | TRefMut t1, TRefMut t2 | TBox t1, TBox t2 ->
      compatible_types t1 t2
  | _ -> false

let is_copyable (typ : Ast.typ) : bool =
  match typ with
  | TInt | TBool | TUnit -> true
  | TRef _ | TRefMut _ | TBox _ | TPlaceholder -> false

let rec is_lvalue_expr (expr : Ast.expr) =
  match expr.expr_desc with
  | Var _ -> true
  | Unop (op, e) -> ( match op with Deref -> is_lvalue_expr e | _ -> false)
  | _ -> false

let is_ref (typ : Ast.typ) : bool =
  match typ with TRef _ | TRefMut _ -> true | _ -> false

let get_inner_type (typ : Ast.typ) : Ast.typ option =
  match typ with TRef t | TRefMut t | TBox t -> Some t | _ -> None

let make_expr (desc : Ast.expr_desc) (typ : Ast.typ) : Ast.expr =
  { expr_desc = desc; expr_type = typ }

let rec type_expr (env : Environment.t) (expr : Ast.expr)
    (r : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match expr.expr_desc with
  | Int _ -> (env, TInt, None)
  | Bool _ -> (env, TBool, None)
  | Var name -> type_var env name
  | Binop (op, e1, e2) -> type_binop env op e1 e2
  | Unop (op, e) -> type_unop env op e r
  | Unit -> (env, TUnit, None)

(* Type check variable reference *)
and type_var (env : Environment.t) (name : string) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match Environment.type_of env name with
  | Some t ->
      if Environment.is_var_alive env name then
        (env, t, Environment.lifetime_of env name)
      else Error.raise_error (OutOfScopeVariable name) None
  | None -> Error.raise_error (UndefinedVariable name) None

and type_binop (env : Environment.t) (op : Ast.binop) (e1 : Ast.expr)
    (e2 : Ast.expr) : Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t1, _ = type_expr env e1 None in
  let env2, t2, _ = type_expr env1 e2 None in

  match op with
  | Add | Sub | Mul | Div | Mod ->
      if t1 = TInt && t2 = TInt then (env2, TInt, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) None
  | Lt | Gt | Le | Ge ->
      if t1 = TInt && t2 = TInt then (env2, TBool, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) None
  | Eq | Ne ->
      if t1 <> t2 then (env2, t1, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) None
  | And | Or ->
      if t1 = TBool && t2 = TBool then (env2, TBool, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) None

(* Type check unary operations *)
and type_unop (env : Environment.t) (op : Ast.unop) (e : Ast.expr)
    (r : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match op with
  | Neg ->
      let env1, t1, _ = type_expr env e r in
      if t1 = TInt then (env1, TInt, None)
      else Error.raise_error (MismatchedUnopError (op, t1)) None
  | Not ->
      let env1, t1, _ = type_expr env e r in
      if t1 = TBool then (env1, TBool, None)
      else Error.raise_error (MismatchedUnopError (op, t1)) None
  | Ref -> type_ref env e r
  | RefMut -> type_ref_mut env e r
  | Deref -> type_deref env e r
  | Move -> type_move env e
  | Box -> type_box env e r
  | Copy -> type_copy env e r
  | EndLifetime -> type_endlifetime env e

(* Type check reference operation *)
and type_ref (env : Environment.t) (e : Ast.expr)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  if is_lvalue_expr e then
    let env1, t, lt1 = type_expr env e lt in
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          Error.raise_error ReadProhibitedError None
        else
          let fresh_lt = Environment.fresh_lifetime env1 in
          Environment.add_outlives env1 propagated_lt fresh_lt;
          (env1, TRef t, Some fresh_lt)
    | None -> Error.raise_error DerefNonLValueError None
  else Error.raise_error DerefNonLValueError None

(* Type check mutable reference operation *)
and type_ref_mut (env : Environment.t) (e : Ast.expr)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  if is_lvalue_expr e then
    let env1, t, lt1 = type_expr env e lt in
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          Error.raise_error WriteProhibitedError None
        else
          let fresh_lt = Environment.fresh_lifetime env1 in
          Environment.add_outlives env1 propagated_lt fresh_lt;
          (env1, TRefMut t, Some fresh_lt)
    | None -> Error.raise_error DerefNonLValueError None
  else Error.raise_error DerefNonLValueError None

(* Type check dereference operation *)
and type_deref (env : Environment.t) (e : Ast.expr)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, lt1 = type_expr env e lt in
  match t with
  | TRef inner | TRefMut inner | TBox inner -> (
      match lt1 with
      | Some propagated_lt -> (env1, inner, Some propagated_lt)
      | None -> Error.raise_error (DerefError t) None)
  | _ -> Error.raise_error (DerefError t) None

(* Type check move operation *)
and type_move (env : Environment.t) (e : Ast.expr) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match e.expr_desc with
  | Var var_name -> (
      match Environment.lookup env var_name with
      | Some binding ->
          if Environment.alive env binding.lifetime then (
            Environment.remove_binding env var_name;
            (env, binding.var_type, Some binding.lifetime))
          else
            Error.raise_error
              (ScopeError ("Variable " ^ var_name ^ " is out of scope"))
              None
      | None -> Error.raise_error (UndefinedVariable var_name) None)
  | _ -> Error.raise_error (OwnershipError "Can only move variables") None

(* Type check box operation *)
and type_box (env : Environment.t) (e : Ast.expr)
    (r : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, _ = type_expr env e r in
  (env1, TBox t, None)

(* Type check copy operation *)
and type_copy (env : Environment.t) (e : Ast.expr)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, lt1 = type_expr env e lt in
  if not (is_copyable t) then
    Error.raise_error
      (OwnershipError ("Type " ^ Error.string_of_type t ^ " is not copyable"))
      None
  else
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          (Error.raise_error WriteProhibitedError) None
        else (env1, t, None)
    | None -> (env1, t, None)

and type_endlifetime (env : Environment.t) (e : Ast.expr) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match e.expr_desc with
  | Var var_name -> (
      match Environment.lookup env var_name with
      | Some binding ->
          Environment.remove_lifetime env binding.lifetime;
          (env, binding.var_type, Some binding.lifetime)
      | None -> Error.raise_error (UndefinedVariable var_name) None)
  | _ ->
      Error.raise_error (OwnershipError "Can only endlifetime of variables")
        None

(* Statement type checking *)

(* Type check a statement, returning the updated environment *)
let rec type_stmt (env : Environment.t) (stmt : Ast.statement) : Environment.t =
  match stmt with
  | Let (mut, name, typ, init_expr) -> type_let env mut name typ init_expr
  | Expr e -> type_expr_stmt env e
  | Block stmts -> type_block env stmts

(* Type check a let binding *)
and type_let (env : Environment.t) (mut : bool) (name : string) (typ : Ast.typ)
    (init_expr : Ast.expr) : Environment.t =
  let env', init_type, lt = type_expr env init_expr None in
  if not (compatible_types typ init_type) then
    Error.raise_error (TypeError (typ, init_type)) None
  else
    match lt with
    | Some propagated_lt ->
        (*propagate lifetime*)
        Environment.fresh_variable env' name typ propagated_lt mut;
        env'
    | None ->
        (*propagate lifetime*)
        let lifetime = Environment.fresh_lifetime env' in
        Environment.fresh_variable env' name typ lifetime mut;
        env'

(* Type check an expression statement *)
and type_expr_stmt (env : Environment.t) (e : Ast.expr) : Environment.t =
  let env', _, _ = type_expr env e None in
  env'

(* Type check a block of statements *)
and type_block (env : Environment.t) (stmts : Ast.statement list) :
    Environment.t =
  (* TODO: Implement block scoping with lifetime handling *)
  let block_env = Environment.copy env in
  let _ = List.fold_left type_stmt block_env stmts in
  env (* Return original environment as block exits scope *)

(* Main type checking function *)

(* Type check a program (list of statements) *)
let type_check (program : Ast.statement list) : unit =
  let env = Environment.create () in
  try
    let _ = List.fold_left type_stmt env program in
    print_endline "Type checking successful!"
  with Error.TypeError (err, loc) ->
    Error.report_error err loc;
    exit 1
