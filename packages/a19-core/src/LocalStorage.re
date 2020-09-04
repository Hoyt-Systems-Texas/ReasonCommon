type t;

[@bs.val] external localStorage_: t = "window.localStorage";

[@bs.send] external setItem_: t => string => string => unit = "setItem";

[@bs.send] external getItem_: t => string => string = "getItem";

let setItem = (key, value) => setItem_(localStorage_, key, value);

let getItem = (key) => getItem_(localStorage_, key);