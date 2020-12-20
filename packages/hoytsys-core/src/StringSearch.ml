(** The goal is to have a very fast immutable searchabe text.  With a design to later be used for more
complex searches and tokenization.  Decided to write my own so it can be contimized to do what I like. *)
module type String_search_type = sig

  (** The type we are stroing the text for. *)
  type t

  (** Used to get the text for the type. *)
  val get_text: t -> string list

  (** A tokenize a string. *)
  val tokenize:  func:('a -> string -> 'a) -> acc:'a -> string  -> 'a
end

module Make_string_search(S: String_search_type) = struct

  type word_position = {

    (** The index of the word in the thext. *)
    index: int;

    (** The id of the document the word is in. *)
    document: int;
  }

  type word = {

    (** The identifier for the word. *)
    identifier: int;

    (** The word text. *)
    word: string;

    (** The documents containing the word. *)
    documents_word: word_position Belt.HashMap.Int.t;
  }

  type pair = {

    (** The key for the string. *)
    key: string;

    (** The value associated with the string. *)
    value: S.t;
  }

  type t = {
    (** The words we are searching for. *)
    words: word array;

    (** The mappigns to a word. *)
    mappinp: word Belt.HashMap.Int.t;

    (** The documents we are matching on. *)
    documents: S.t Belt.HashMap.Int.t;

  }

end