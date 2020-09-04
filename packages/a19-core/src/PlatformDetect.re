type t =
    | Unknown
    | IOS
    ;

[@bs.val] external platform_: t = "window.navigator";
[@bs.get] external get_platform_: t => string = "platform";

let getPlatform = () => {
    switch (get_platform_(platform_)) {
        | "iPad Simulator"
        | "iPhone Simulator"
        | "iPad"
        | "iPhone"
        | "iPod" => IOS
        | _ => Unknown
    }
};