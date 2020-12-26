(** The goal is to have a very fast immutable searchabe text.  With a design to later be used for more
complex searches and tokenization.  Decided to write my own so it can be contimized to do what I like. *)
module type String_search_type = sig

  (** The type we are stroing the text for. *)
  type t

  (** Used to get the text for the type. *)
  val get_text: t -> string

  (** A tokenize a string. *)
  val tokenize:  func:('a -> string -> int -> 'a) -> acc:'a -> string  -> 'a

end

module Make_string_search(S: String_search_type): sig
  
  type t

  (** Makes a new string search. *)
  val make: S.t array -> t

  val search: string -> t -> S.t array

end