include List

let generate t =
  List.fold_left (fun all clazz -> all ^ " " ^ clazz) "" t