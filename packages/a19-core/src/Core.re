
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