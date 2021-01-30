type uint8array
type text_decoder

(* Used to get a raw string array with no encoding. *)
external make_buffer : Fetch.arrayBuffer -> uint8array = "Uint8Array" [@@bs.new]
external make_text_decoder:  string -> text_decoder = "TextDecoder" [@@bs.new]
external decode_text : text_decoder -> uint8array -> string = "decode" [@@bs.send]

external get_buffer_source : Js_typed_array2.Int8Array.t -> Fetch.bufferSource= "buffer" [@@bs.get]

(** Takes in the array buffer and returns a string that hasn't been modified at all.*)
let to_raw_string buffer =
  let raw_buffer = make_buffer buffer in
  let decoder = make_text_decoder "x-user-defined" in
  decode_text decoder raw_buffer

let to_buffer value =
  let module A = Js_typed_array2.Int8Array in
  let length = String.length value in
  let raw_buffer = A.fromLength length in
  let set_buffer = A.unsafe_set raw_buffer in
  String.iteri (fun idx s ->
    set_buffer idx (Char.code s)) value;
  raw_buffer
    |> get_buffer_source

