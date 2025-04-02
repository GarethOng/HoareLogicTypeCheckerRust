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
    mutable lifetime_to_name : (string, string) Hashtbl.t;
  }

  let create () =
    {
      bindings = Hashtbl.create 16;
      lifetime_ctx = { next_id = 0; alive = []; outlives = [] };
      lifetime_to_name = Hashtbl.create 16;
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
      { var_type = typ; lifetime; mutable_var = mut };
    Hashtbl.replace env.lifetime_to_name lifetime name

  let add_outlives env a b =
    env.lifetime_ctx <-
      { env.lifetime_ctx with outlives = (a, b) :: env.lifetime_ctx.outlives }

  let rec outlives env a b =
    List.exists (fun (x, y) -> x = a && y = b) env.lifetime_ctx.outlives
    || List.exists
         (fun (x, y) -> x = a && outlives env y b)
         env.lifetime_ctx.outlives

  let find_overlapping_lifetimes env lifetime =
    if not (List.mem lifetime env.lifetime_ctx.alive) then None
    else
      let other_lifetimes =
        List.filter (fun lt -> lt <> lifetime) env.lifetime_ctx.alive
      in
      let overlapping =
        List.filter
          (fun other_lt -> outlives env lifetime other_lt)
          other_lifetimes
      in
      if overlapping = [] then None else Some overlapping

  let lookup env name =
    try Some (Hashtbl.find env.bindings name) with Not_found -> None

  let exists env name = Hashtbl.mem env.bindings name
  let remove_binding env name = Hashtbl.remove env.bindings name

  let get_all_bindings env =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) env.bindings []

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

  let mark_as_borrowed env name new_lifetime =
    match lookup env name with
    | Some binding ->
        add_outlives env binding.lifetime new_lifetime;
        true
    | None -> false

  let type_of env name =
    match lookup env name with
    | Some binding -> Some binding.var_type
    | None -> None

  let is_mutable env name =
    match lookup env name with
    | Some binding -> binding.mutable_var
    | None -> false

  let lifetime_of env name =
    match lookup env name with
    | Some binding -> Some binding.lifetime
    | None -> None

  let get_lifetime_context env = env.lifetime_ctx

  let write_prohibited env lifetime =
    if not (List.mem lifetime env.lifetime_ctx.alive) then true
    else
      match find_overlapping_lifetimes env lifetime with
      | None -> false
      | Some _ -> true

  let read_prohibited env lifetime =
    if not (List.mem lifetime env.lifetime_ctx.alive) then true
    else
      match find_overlapping_lifetimes env lifetime with
      | None -> false
      | Some non_outliving_lifetimes ->
          (* Check if any non-outliving lifetime corresponds to a mutable reference *)
          let has_mut_ref =
            List.exists
              (fun lt ->
                match Hashtbl.find_opt env.lifetime_to_name lt with
                | None -> false
                | Some var_name -> (
                    match lookup env var_name with
                    | None -> false
                    | Some binding -> (
                        match binding.var_type with
                        | TRefMut _ -> true
                        | _ -> false)))
              non_outliving_lifetimes
          in
          has_mut_ref
end
