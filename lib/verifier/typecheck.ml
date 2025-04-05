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

let rec type_expr (env : Environment.t) (expr : Ast.expr)
    (r : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match expr.expr_desc with
  | Int _ -> (env, TInt, None)
  | Bool _ -> (env, TBool, None)
  | Var name -> type_var env name expr.expr_loc
  | Binop (op, e1, e2) -> type_binop env op e1 e2 expr.expr_loc
  | Unop (op, e) -> type_unop env op e expr.expr_loc r
  | Unit -> (env, TUnit, None)

and type_var (env : Environment.t) (name : string) (loc : Ast.location) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match Environment.type_of env name with
  | Some t ->
      if Environment.is_var_alive env name then
        (env, t, Environment.lifetime_of env name)
      else Error.raise_error (OutOfScopeVariable name) loc
  | None -> Error.raise_error (UndefinedVariable name) loc

and type_binop (env : Environment.t) (op : Ast.binop) (e1 : Ast.expr)
    (e2 : Ast.expr) (loc : Ast.location) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t1, _ = type_expr env e1 None in
  let env2, t2, _ = type_expr env1 e2 None in

  match op with
  | Add | Sub | Mul | Div | Mod ->
      if t1 = TInt && t2 = TInt then (env2, TInt, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) loc
  | Lt | Gt | Le | Ge ->
      if t1 = TInt && t2 = TInt then (env2, TBool, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) loc
  | Eq | Ne ->
      if t1 <> t2 then (env2, t1, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) loc
  | And | Or ->
      if t1 = TBool && t2 = TBool then (env2, TBool, None)
      else Error.raise_error (MismatchedBinopError (op, t1, t2)) loc

and type_unop (env : Environment.t) (op : Ast.unop) (e : Ast.expr)
    (loc : Ast.location) (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match op with
  | Neg ->
      let env1, t1, _ = type_expr env e lt in
      if t1 = TInt then (env1, TInt, None)
      else Error.raise_error (MismatchedUnopError (op, t1)) loc
  | Not ->
      let env1, t1, _ = type_expr env e lt in
      if t1 = TBool then (env1, TBool, None)
      else Error.raise_error (MismatchedUnopError (op, t1)) loc
  | Ref -> type_ref env e loc lt
  | RefMut -> type_ref_mut env e loc lt
  | Deref -> type_deref env e loc lt
  | Move -> type_move env e loc
  | Box -> type_box env e lt
  | Copy -> type_copy env e loc lt
  | EndLifetime -> type_endlifetime env e loc

and type_ref (env : Environment.t) (e : Ast.expr) (loc : Ast.location)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  if is_lvalue_expr e then
    let env1, t, lt1 = type_expr env e lt in
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          Error.raise_error ReadProhibitedError loc
        else
          let fresh_lt = Environment.fresh_lifetime env1 in
          Environment.add_outlives env1 propagated_lt fresh_lt;
          (env1, TRef t, Some fresh_lt)
    | None -> Error.raise_error DerefNonLValueError loc
  else Error.raise_error DerefNonLValueError loc

and type_ref_mut (env : Environment.t) (e : Ast.expr) (loc : Ast.location)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  if is_lvalue_expr e then
    let env1, t, lt1 = type_expr env e lt in
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          Error.raise_error WriteProhibitedError loc
        else
          let fresh_lt = Environment.fresh_lifetime env1 in
          Environment.add_outlives env1 propagated_lt fresh_lt;
          (env1, TRefMut t, Some fresh_lt)
    | None -> Error.raise_error DerefNonLValueError loc
  else Error.raise_error DerefNonLValueError loc

and type_deref (env : Environment.t) (e : Ast.expr) (loc : Ast.location)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, lt1 = type_expr env e lt in
  match t with
  | TRef inner | TRefMut inner | TBox inner -> (
      match lt1 with
      | Some propagated_lt -> (env1, inner, Some propagated_lt)
      | None -> Error.raise_error (DerefError t) loc)
  | _ -> Error.raise_error (DerefError t) loc

and type_move (env : Environment.t) (e : Ast.expr) (loc : Ast.location) :
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
              loc
      | None -> Error.raise_error (UndefinedVariable var_name) loc)
  | _ -> Error.raise_error (OwnershipError "Can only move variables") loc

and type_box (env : Environment.t) (e : Ast.expr)
    (r : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, _ = type_expr env e r in
  (env1, TBox t, r)

and type_copy (env : Environment.t) (e : Ast.expr) (loc : Ast.location)
    (lt : propagated_lifetime option) :
    Environment.t * Ast.typ * propagated_lifetime option =
  let env1, t, lt1 = type_expr env e lt in
  if not (is_copyable t) then
    Error.raise_error
      (OwnershipError ("Type " ^ Error.string_of_type t ^ " is not copyable"))
      loc
  else
    match lt1 with
    | Some propagated_lt ->
        if Environment.read_prohibited env1 propagated_lt then
          (Error.raise_error WriteProhibitedError) loc
        else (env1, t, None)
    | None -> (env1, t, None)

and type_endlifetime (env : Environment.t) (e : Ast.expr) (loc : Ast.location) :
    Environment.t * Ast.typ * propagated_lifetime option =
  match e.expr_desc with
  | Var var_name -> (
      match Environment.lookup env var_name with
      | Some binding ->
          Environment.remove_lifetime env binding.lifetime;
          (env, binding.var_type, Some binding.lifetime)
      | None -> Error.raise_error (UndefinedVariable var_name) loc)
  | _ ->
      Error.raise_error (OwnershipError "Can only endlifetime of variables") loc

let rec type_stmt (env : Environment.t) (stmt : Ast.statement) : Environment.t =
  match stmt with
  | Let (mut, name, typ, init_expr, _) -> type_let env mut name typ init_expr
  | Expr (e, _) -> type_expr_stmt env e
  | Block (stmts, _) -> type_block env stmts

and type_let (env : Environment.t) (mut : bool) (name : string) (typ : Ast.typ)
    (init_expr : Ast.expr) : Environment.t =
  let env', init_type, lt = type_expr env init_expr None in
  if not (compatible_types typ init_type) then
    Error.raise_error (TypeError (typ, init_type)) init_expr.expr_loc
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

and type_expr_stmt (env : Environment.t) (e : Ast.expr) : Environment.t =
  let env', _, _ = type_expr env e None in
  env'

and type_block (env : Environment.t) (stmts : Ast.statement list) :
    Environment.t =
  (* TODO: Implement block scoping with lifetime handling *)
  let block_env = Environment.copy env in
  let _ = List.fold_left type_stmt block_env stmts in
  env (* Return original environment as block exits scope *)

let type_check (program : Ast.statement list) : unit =
  let env = Environment.create () in
  try
    let _ = List.fold_left type_stmt env program in
    print_endline "Type checking successful!"
  with Error.TypeError (err, loc) ->
    Error.report_error err loc;
    exit 1
