(** Takes a fetch blob and converts it to a raw binary string. *)
val to_raw_string: Fetch.arrayBuffer -> string

(** Takes a binary string and converts to to a aarray buffer with out any changes. *)
val to_buffer: string -> Fetch.bufferSource