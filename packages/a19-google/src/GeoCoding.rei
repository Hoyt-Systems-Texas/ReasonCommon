open A19Core.Model.Address;
open A19Core.Model.Location;


type viewPort = {
    northeast: geoCode,
    southwest: geoCode,
};

type geometry = {
    location: geoCode,
    locationType: string,
    viewPort: viewPort,
}

type okResult = {
    viewPort: viewPort,
}

type matchingAddress = {
    formattedAddress: string,
    geometry: geometry,
};

type geoCodingResult =
    | Ok(array(matchingAddress))
    | ZeroResults
    | OveryDailyLimit
    | OveryQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError
    | UnknownResponse
    ;

let getGeoCoding : string => address => Js.Promise.t(A19Core.Core.resultMonad(geoCodingResult));
