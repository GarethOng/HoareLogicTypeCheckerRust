module Error = struct
  type error_type =
    | TypeError of Ast.typ * Ast.typ
    | UndefinedVariable of string
    | MismatchedBinopError of Ast.binop * Ast.typ * Ast.typ
    | MismatchedUnopError of Ast.unop * Ast.typ
    | OutOfScopeVariable of string
    | MutabilityError of string
    | OwnershipError of string
    | LifetimeError of string * string
    | ImmutableBorrowError of string
    | BoxError of string
    | DerefError of Ast.typ
    | ScopeError of string
    | DerefNonLValueError
    | WriteProhibitedError
    | ReadProhibitedError
    | MoveNonLValueError

  exception TypeError of error_type * Lexing.position option

  let rec string_of_type (typ : Ast.typ) : string =
    match typ with
    | Ast.TInt -> "i32"
    | Ast.TBool -> "bool"
    | Ast.TUnit -> "()"
    | Ast.TRef t -> "&" ^ string_of_type t
    | Ast.TRefMut t -> "&mut " ^ string_of_type t
    | Ast.TBox t -> "Box<" ^ string_of_type t ^ ">"
    | Ast.TPlaceholder -> "invalid"

  let string_of_binop (binop : Ast.binop) : string =
    match binop with
    (* Arithmetic *)
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    (* Comparison *)
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    (* Logical *)
    | Eq -> "=="
    | Ne -> "!="
    | And -> "&&"
    | Or -> "||"

  let string_of_unop (unop : Ast.unop) : string =
    match unop with
    | Neg -> "-"
    | Not -> "!"
    | Ref -> "&"
    | RefMut -> "&mut "
    | Deref -> "*"
    | Move -> "move "
    | Box -> "box "
    | Copy -> "copy "
    | EndLifetime -> "endlifetime "

  let location_string (pos : Lexing.position option) : string =
    match pos with
    | Some pos ->
        Printf.sprintf "line %d, character %d" pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
    | None -> "unknown location"

  let error_message (err : error_type) (pos : Lexing.position option) : string =
    let loc_str = location_string pos in
    match err with
    | TypeError (expected, actual) ->
        Printf.sprintf "At %s: Type error - expected %s but got %s" loc_str
          (string_of_type expected) (string_of_type actual)
    | UndefinedVariable name ->
        Printf.sprintf "At %s: Undefined variable '%s'" loc_str name
    | OutOfScopeVariable name ->
        Printf.sprintf "At %s: Variable out of scope '%s'" loc_str name
    | MutabilityError msg ->
        Printf.sprintf "At %s: Mutability error - %s" loc_str msg
    | OwnershipError msg ->
        Printf.sprintf "At %s: Ownership error - %s" loc_str msg
    | LifetimeError (a, b) ->
        Printf.sprintf "At %s: Lifetime error - '%s doesn't outlive '%s" loc_str
          a b
    | MismatchedBinopError (operand, x, y) ->
        Printf.sprintf "At %s: Undefined binary operation - '%s '%s '%s" loc_str
          (string_of_type x) (string_of_binop operand) (string_of_type y)
    | MismatchedUnopError (operand, x) ->
        Printf.sprintf "At %s: Undefined unary operation : '%s on type: '%s"
          loc_str (string_of_unop operand) (string_of_type x)
    | ImmutableBorrowError msg ->
        Printf.sprintf "At %s: Borrow error - %s" loc_str msg
    | BoxError msg -> Printf.sprintf "At %s: Box error - %s" loc_str msg
    | DerefError t ->
        Printf.sprintf
          "At %s: Dereference error - Trying to dereference of type: '%s"
          loc_str (string_of_type t)
    | ScopeError msg -> Printf.sprintf "At %s: Scope error - %s" loc_str msg
    | DerefNonLValueError ->
        Printf.sprintf "At %s: Attempting to dereference non L-Value" loc_str
    | WriteProhibitedError -> Printf.sprintf "At %s: write prohibited" loc_str
    | ReadProhibitedError -> Printf.sprintf "At %s: read prohibited" loc_str
    | MoveNonLValueError ->
        Printf.sprintf "At %s: Attempting to move non L-Value" loc_str

  let report_error (err : error_type) (pos : Lexing.position option) : unit =
    prerr_endline (error_message err pos)

  let raise_error err pos = raise (TypeError (err, pos))
end
