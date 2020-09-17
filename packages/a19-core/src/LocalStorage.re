type t;

[@bs.val] external localStorage_: t = "window.localStorage";

[@bs.send] external setItem_: t => string => string => unit = "setItem";

[@bs.send] external getItem_: t => string => Js.Nullable.t(string) = "getItem";

let setItem = (key, value) => setItem_(localStorage_, key, value);

let getItem = (key) => Js.Nullable.toOption(getItem_(localStorage_, key));