module R = Core.ResultMonad

module type Cache_value_type = sig
  type t

  val load: unit -> t R.t Js.Promise.t
end

module Make_cached_value(T: Cache_value_type) : sig

  type t

  val make: unit -> t
  (** Creates a new cached value. *)

  val get: t -> T.t option Js.Promise.t
  (** Gets the cached value. Returns None if there is an error getting it.*)

end