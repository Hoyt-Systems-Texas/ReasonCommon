let root_id = 0

module type Tree_lookup = sig
  type value

  val get_key: value -> int
  (** Used to get the key of the value. *)

  val get_parent: value -> int option
  (** Used to get the parent value of the key. *)
end

module Make_tree(L: Tree_lookup) = struct

  type t = {
    lookup: L.value list Belt.HashMap.Int.t;
    values: L.value Belt.HashMap.Int.t;
  }

  let get_parent_id value =
    match L.get_parent value with
    | Some parent_id -> parent_id
    | None -> root_id

  let add_child map lookup parent_id value =
    let module H = Belt.HashMap.Int in
    let key = L.get_key value in
    let add_child () =
      match H.get lookup parent_id with 
      | Some v -> 
        H.set lookup parent_id @@ value::v
      | None ->
        H.set lookup parent_id [value] in
    let update_lookup () =
      H.set map (L.get_key value) value in 
    match H.get map key with 
    | Some v -> (
      let old_parent_id = get_parent_id v in
      update_lookup ();
      if old_parent_id = parent_id then
        (
          match H.get lookup parent_id with
          | Some v ->
            H.set lookup parent_id @@ List.map (fun n -> if L.get_key value = L.get_key n then
              value else n) v
          | None ->
            H.set lookup parent_id [value]
        )
      else 
        update_lookup ();
        match H.get lookup old_parent_id with
        | Some v ->
          H.set lookup parent_id @@ List.filter (fun n -> not (L.get_key value = L.get_key n)) v;
        | None -> add_child ()
    ) 
    | None -> 
      update_lookup ();
      add_child ()

  let make values =
    let module H = Belt.HashMap.Int in
    let lookup = H.make ~hintSize:20 in
    let map = H.make ~hintSize:50 in
    List.iter (fun value ->
      add_child map lookup (match L.get_parent value with
      | Some parent_id -> parent_id
      | None -> root_id)
      value) values;
    {
      lookup;
      values = map;
    }
  
  let get t key =
    let module H = Belt.HashMap.Int in
    H.get t.values key 

  let children t key =
    Belt.HashMap.Int.get t.lookup key
    |. Belt.Option.getWithDefault []

  let add_or_update t value =
    add_child t.values t.lookup (match L.get_parent value with
    | Some parent_id -> parent_id
    | None -> root_id) value

  let remove t key =
    let module H = Belt.HashMap.Int in
    match H.get t.values key with
    | Some node ->
      H.remove t.values key;
      let parent_id = get_parent_id node in
      (match H.get t.lookup parent_id with 
      | Some v ->
        H.set t.lookup parent_id @@ List.filter (fun v -> L.get_key v = key) v
          @ (match H.get t.lookup key with
            | Some v -> v
            | None -> [])
      | None -> ())
    | None -> ()
end