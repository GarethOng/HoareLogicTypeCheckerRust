module Environment = struct
  type lifetime = string
  type binding = { var_type : Ast.typ; lifetime : lifetime; mutable_var : bool }

  type lifetime_context = {
    next_id : int;
    alive : lifetime list;
    outlives : (lifetime * lifetime) list;
  }

  type t = {
    mutable bindings : (string, binding) Hashtbl.t;
    mutable lifetime_ctx : lifetime_context;
  }

  let create () =
    {
      bindings = Hashtbl.create 16;
      lifetime_ctx = { next_id = 0; alive = []; outlives = [] };
    }

  let fresh_lifetime env =
    let id = env.lifetime_ctx.next_id in
    let lifetime = "'t" ^ string_of_int id in
    env.lifetime_ctx <-
      {
        env.lifetime_ctx with
        next_id = id + 1;
        alive = lifetime :: env.lifetime_ctx.alive;
      };
    lifetime

  let fresh_variable env name typ lifetime mut =
    Hashtbl.replace env.bindings name
      { var_type = typ; lifetime; mutable_var = mut }

  (* Add an outlives relationship: 'a outlives 'b *)
  let add_outlives env a b =
    env.lifetime_ctx <-
      { env.lifetime_ctx with outlives = (a, b) :: env.lifetime_ctx.outlives }

  (* Check if lifetime 'a outlives lifetime 'b *)
  let rec outlives env a b =
    (* Direct outlives relationship *)
    List.exists (fun (x, y) -> x = a && y = b) env.lifetime_ctx.outlives
    ||
    (* Transitive outlives relationship *)
    List.exists
      (fun (x, y) -> x = a && outlives env y b)
      env.lifetime_ctx.outlives

  (* Look up a binding in the environment *)
  let lookup env name =
    try Some (Hashtbl.find env.bindings name) with Not_found -> None

  (* Check if a variable exists in the environment *)
  let exists env name = Hashtbl.mem env.bindings name

  (* Remove a binding from the environment *)
  let remove_binding env name = Hashtbl.remove env.bindings name

  (* Get all bindings in the environment *)
  let get_all_bindings env =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) env.bindings []

  (* Create a deep copy of the environment *)
  let copy env =
    let new_env = create () in
    new_env.lifetime_ctx <-
      {
        next_id = env.lifetime_ctx.next_id;
        alive = env.lifetime_ctx.alive;
        outlives = env.lifetime_ctx.outlives;
      };
    let new_bindings = Hashtbl.create (Hashtbl.length env.bindings) in
    Hashtbl.iter (fun k v -> Hashtbl.add new_bindings k v) env.bindings;
    new_env.bindings <- new_bindings;
    new_env

  (* Mark a variable as borrowed with a new lifetime *)
  let mark_as_borrowed env name new_lifetime =
    match lookup env name with
    | Some binding ->
        (* Add outlives relationship - original lifetime outlives borrow lifetime *)
        add_outlives env binding.lifetime new_lifetime;
        true
    | None -> false

  (* Get the type of a variable *)
  let type_of env name =
    match lookup env name with
    | Some binding -> Some binding.var_type
    | None -> None

  (* Check if a variable is mutable *)
  let is_mutable env name =
    match lookup env name with
    | Some binding -> binding.mutable_var
    | None -> false

  (* Get the lifetime of a variable *)
  let lifetime_of env name =
    match lookup env name with
    | Some binding -> Some binding.lifetime
    | None -> None

  (* Get the current lifetime context *)
  let get_lifetime_context env = env.lifetime_ctx
end
