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

    (** The length of the word.  Saved here to make it easier to get the value. *)
    word_length: int;

    (** The documents containing the word. A word can be in the same contents multiple times. The key 
    is the doucment id. *)
    documents_word: word_position Belt.HashMap.Int.t;
  }

  type t = {
    (** The words we are searching for.  The array needs to be sorted and will be used for a binary search. *)
    words: word array;

    (** The total number of words in the array. *)
    number_of_words: int;

    (** The middle of the words. *)
    middle: int;

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
              word_length=String.length word;
              documents_word=I_map.make ~hintSize:3;
            } in 
            I_map.set document.documents_word document_id word_pos;
          );
          words_map))

  let make documents =
    let module S_map = Belt.HashMap.String in
    let word_documents = tokenize_documents documents in
    let words = S_map.valuesToArray word_documents in
    let middle = Array.length words / 2 in
    Array.sort (fun d1 d2 -> String.compare d1.word d2.word) words;
    {
      words;
      documents;
      number_of_words=Array.length words;
      middle;
    }

  (** Finds the starting point for the search using binary search. *)
  let find_match_start word t =
    let word_length = String.length word in
    let rec search_bin idx start end_ =
      let documents = Array.get t.words idx in
      let num = String.compare documents.word word in
      if num = 0 then
        Some idx
      else if num < 0 then
        if word_length > documents.word_length then
          let half = (start + idx) / 2 in
          if idx = half then
            let substring = String.sub documents.word 0 word_length in
            if substring = word then
              Some idx 
            else 
              None
          else
            search_bin half start idx
        else
          let half = (start + idx) / 2 in
          if half = idx then
            None
          else 
            search_bin half start idx
      else
        if word_length > documents.word_length then
          if end_ = idx then
            let substring = String.sub documents.word 0 word_length in
            if substring = word then
              Some idx
            else
              None
          else 
            let total = end_ + idx in
            let half = (total / 2) + total mod 2 in
            search_bin half idx end_
        else
          if end_ = idx then
            None
          else
            let total = end_ + idx in
            let half = (total / 2 ) + total mod 2 in
            search_bin half idx end_
        in
    search_bin t.middle 0 t.number_of_words

  let search text t =
    let module I_map = Belt.HashMap.Int in
    let words = S.tokenize ~func:(fun words word _ -> word::words) ~acc:[] text
      |> Belt.List.reverse
    in
    (** The potential matching documents. *)
    let idx = Belt.List.map words (fun word -> (word, find_match_start word t)) in
    (** Check to see if any of them are None and if they are stop. *)
    if Belt.List.some idx (fun (_, id) -> id = None) then
      []
    else 
      let rec find_words documents idx =
        match idx with
        | head::tail -> 
          let (word, start_idx) = head in
          let text_length = String.length word in
          let rec dec_match idx documents =
            (match Belt.Array.get t.words idx with
              | Some word_docs ->
                if word_docs.word_length >= text_length then
                  let d = word_docs.documents_word in
                  let next_words = match documents with
                  | Some current_matches -> 
                    I_map.forEach current_matches (fun id _ ->
                      if I_map.has d id then
                        ()
                      else
                        I_map.remove d id);
                    current_matches
                  | None -> I_map.copy d in
                    find_words (Some next_words) tail
                else
                  I_map.make ~hintSize:0
              | None ->
                I_map.make ~hintSize:0)
          in
          dec_match start_idx documents
        | [] -> I_map.make ~hintSize:0
      in
      []

end