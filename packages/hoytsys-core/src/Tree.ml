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

  let make values =
    let module H = Belt.HashMap.Int in
    let map = H.make ~hintSize:20 in
    let lookup = List.fold_left (fun map value ->
      match L.get_parent value with
      | Some parent -> (
        match H.get map parent with
        | Some v ->
          H.set map parent @@ value::v;
          map
        | None ->
          H.set map parent [value];
          map)
      | None -> map) map @@ List.rev values in
    let map = H.make ~hintSize:50 in
    let values = List.fold_left (fun map value ->
      let key = L.get_key value in
      H.set map key value;
      map) map values in
    {
      lookup;
      values;
    }
  
  let get t key =
    let module H = Belt.HashMap.Int in
    H.get t.values key 

  let children t key =
    Belt.HashMap.Int.get t.lookup key
    |. Belt.Option.getWithDefault []

end