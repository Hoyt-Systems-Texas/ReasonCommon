module R = Core.ResultMonad

module type Cache_value_type = sig
  type t

  val load: unit -> t R.t Js.Promise.t

end

module Make_cached_value(T: Cache_value_type) = struct

  type load_result  =
  | Not_loaded
  | Loading
  | Load of T.t
  | Failed

  type pending_requests = (T.t option -> unit) list

  type t = {
    value: load_result ref;
    requests: pending_requests ref;
  }

  let make () =
    {
      value=ref Not_loaded;
      requests= ref [];
    }

  let update_with_value t =
    let v = match !(t.value) with 
    | Not_loaded
    | Loading
    | Failed -> 
      None
    | Load v -> Some(v) in
    let handlersRef = t.requests in
    let handlers = !handlersRef in
    handlersRef := [];
    Belt.List.forEach handlers (fun h -> h(v))
  
  let get t =
    let value = t.value in
    match !value with
      | Not_loaded
      | Failed ->
        let requests = t.requests in
        let promise = Js.Promise.make (fun ~resolve ~reject -> 
          let r = (fun (v: T.t option) -> ((resolve v) [@bs])) in
          requests := [r];
          ()) in
        Js.Promise.(
          T.load ()
          |> then_(fun result -> 
            match result with
            | R.Success result ->
              value := Load result;
              update_with_value t;
              resolve()
            | _ ->
              value := Failed;
              update_with_value t;
              resolve()))
          |> ignore;
        promise
      | Load v ->
        Js.Promise.make(fun ~resolve ~reject ->
          ((resolve (Some v)) [@bs]))
      | Loading ->
        let requests = t.requests in
        Js.Promise.make(fun ~resolve ~reject ->
          let r = (fun (v: T.t option) -> ((resolve v) [@bs])) in
            requests := [r])

end