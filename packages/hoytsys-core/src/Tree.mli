module type Tree_lookup = sig
  type value

  val get_key: value -> int
  (** Used to get the key of the value. *)

  val get_parent: value -> int option
  (** Used to get the parent value of the key. *)
end

module Make_tree(L: Tree_lookup) : sig
  type t

  val make :  L.value list  -> t
  (** Creates a new tree from an initial value. [reduce] a reduce funciton for the structure.*)

  val get : t -> int -> L.value option
  (** Used to get a node at a position. *)

  val children : t -> int -> L.value list
  (** Used to get the children of the node. *)
end 