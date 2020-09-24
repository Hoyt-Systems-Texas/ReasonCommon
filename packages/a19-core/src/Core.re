
type resultMonad('a) = 
    | Success('a)
    | Error(array(string))
    | Busy
    | AccessDenied
    ;

module Decode = {
    open Json.Decode;

    let decodeError = json => {
        let errors = json |> array(string)
        Error(errors)
    };
    
    let decodeResultMonad = (json, resultType) => {
        json |> oneOf([
            field("success", json => Success(resultType(json))),
            field("error", decodeError), 
            field("busy", _ => Busy),
            field("accessDenied", _ => AccessDenied),
        ]);
    }

    let decodeEmptyResult = json => decodeResultMonad(json, (_) => ());
}

module UpdateValue = {
    type t =
        | Unknown
        | New
        | Updated
        | Same
        ;
}

let emptyGuid = "00000000-0000-0000-0000-000000000000";

let parseTimeString = timeString => {
    let date = MomentRe.momentDefaultFormat("1970-1-1 " ++ timeString);
    if (MomentRe.Moment.isValid(date)) {
        Some(date)
    } else {
        None
    }
}

let getTotalMinutes = date => {
    MomentRe.Moment.toUnix(date) / 60
}

type asyncLoadState('a) =
    | Pending
    | Loading
    | Loaded('a)
    | LoadFailed
    ;

let notADigit = [%re "/[^0-9]/g"]

let removeNonDigits = (value) => {
    Js.String.replaceByRe(notADigit, "", value);
}

let whiteSpaceRegex = [%re "/\s+/"];

let tokenizeString = (value) => {
    Js.String.splitByRe(
        whiteSpaceRegex,
         Js.String.toLocaleLowerCase(Js.String.trim(value)))
         -> Array.to_list
         -> Belt.List.keepMap(a => a);
}

module FormState = {
    type t = 
        | Loading
        | Edit
        | Saving
        | Error(list(string))
        ;
}

module VisibleState {
    type t =
    | Show
    | Hide
    ;

    let toggle = (t) => switch (t) {
        | Show => Hide
        | Hide => Show
    };
}

module OptionExt {

    let map = (t, f) => {
        switch (t) {
            | Some(a) => Some(f(a))
            | None => None
        }
    }
}
