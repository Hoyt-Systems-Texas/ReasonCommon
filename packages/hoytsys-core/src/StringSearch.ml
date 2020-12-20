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

module Make_string_search(S: String_search_type) = struct

  type word_position = {

    (** The index of the word in the thext. *)
    index: int array;

    (** The id of the document the word is in. *)
    document: int;
  }

  type word = {

    (** The word text. *)
    word: string;

    (** The documents containing the word. A word can be in the same contents multiple times. *)
    documents_word: word_position Belt.HashMap.Int.t;
  }

  type t = {
    (** The words we are searching for.  The array needs to be sorted and will be used for a binary search. *)
    words: word array;

    (** The documents we are matching on. Going to index the documents based on their position. *)
    documents: S.t array;

  }

  (** Used to tokenize the data. *)
  let tokenize_documents documents =
    let module S_map = Belt.HashMap.String in
    let module I_map = Belt.HashMap.Int in
    let words_map = S_map.make ~hintSize:1000 in
    Belt.Array.reduceWithIndex documents words_map
      (fun words_map document document_id ->
        let document_words = 
        document
        |> S.get_text
        |> S.tokenize ~acc:(S_map.make ~hintSize:5)  ~func:(fun document_words word word_idx -> 
          (match S_map.get document_words word with 
          | Some positions -> S_map.set document_words word @@ word_idx::positions
          | None -> S_map.set document_words word [word_idx]);
          document_words) in 
        S_map.reduce document_words words_map (fun words_map word idxs ->
          let word_pos = {
            index = Belt.List.reverse idxs |> Array.of_list;
            document = document_id;
          } in
          (match S_map.get words_map word with
          | Some documents ->
            I_map.set documents.documents_word document_id word_pos;
          | None -> 
            let document = {
              word;
              documents_word=I_map.make ~hintSize:3;
            } in 
            I_map.set document.documents_word document_id word_pos;
          );
          words_map))

  let make documents =
    let module S_map = Belt.HashMap.String in
    let word_documents = tokenize_documents documents in
    let words = S_map.valuesToArray word_documents in
    Array.sort (fun d1 d2 -> String.compare d1.word d2.word) words;
    {
      words;
      documents;
    }

end