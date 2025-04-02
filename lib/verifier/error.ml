module Error = struct
  type error_type =
    | TypeError of Ast.typ * Ast.typ
    | UndefinedVariable of string
    | MutabilityError of string
    | OwnershipError of string
    | LifetimeError of string * string
    | ImmutableBorrowError of string
    | BoxError of string
    | DerefError of string
    | ScopeError of string

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
    | MutabilityError msg ->
        Printf.sprintf "At %s: Mutability error - %s" loc_str msg
    | OwnershipError msg ->
        Printf.sprintf "At %s: Ownership error - %s" loc_str msg
    | LifetimeError (a, b) ->
        Printf.sprintf "At %s: Lifetime error - '%s doesn't outlive '%s" loc_str
          a b
    | ImmutableBorrowError msg ->
        Printf.sprintf "At %s: Borrow error - %s" loc_str msg
    | BoxError msg -> Printf.sprintf "At %s: Box error - %s" loc_str msg
    | DerefError msg ->
        Printf.sprintf "At %s: Dereference error - %s" loc_str msg
    | ScopeError msg -> Printf.sprintf "At %s: Scope error - %s" loc_str msg

  let report_error (err : error_type) (pos : Lexing.position option) : unit =
    prerr_endline (error_message err pos)

  let raise_error err pos = raise (TypeError (err, pos))
end
