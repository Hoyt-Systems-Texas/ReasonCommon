let nullable_string s =
  if Js.String.length s = 0 then
    None
  else
    Some s

let empty_string s =
  match s with
  | Some s -> s
  | None -> ""