
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
}