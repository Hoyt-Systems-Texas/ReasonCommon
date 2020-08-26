open A19Core.Model.Address;
open A19Core.Model.Location;

let geocodingUrl = "https://maps.googleapis.com/maps/api/geocode/json?address=";

type geoCodeOk = {
    formattedAddress: string,
};

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

type geoCodingResult =
    | Ok
    | ZeroResults
    | OveryDailyLimit
    | OveryQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError
    | UnknownResponse
    ;

module Decode {
    let decodeGeoCode = (json) => {
        Json.Decode.(
        {
            lat: json |> field("lat", float),
            lng: json |> field("lng", float),
        });
    };

    let decodeViewPort = (json) => {
        Json.Decode.(
        {
            northeast: json |> field("northeast", decodeGeoCode),
            southwest: json |> field("southwest", decodeGeoCode),
        })
    };

    let decodeGeometry = (json) => {
        Json.Decode.(
        {
            location: json |> field("location", decodeGeoCode),
            locationType: json |> field("location_type", string),
            viewPort: json |> field("viewport", decodeViewPort)
        })
    };

    let decodeGeoCodingResult = (json) => {
        let status = json |> Json.Decode.field("status", Json.Decode.string);
        switch (status) {
            | "OK" => Ok
            | "ZERO_RESULT" => ZeroResults
            | "OVER_DAILY_LIMIT" => OveryDailyLimit
            | "OVER_QUERY_LIMIT" => OveryQueryLimit
            | "REQUEST_DENIED" => RequestDenied
            | "INVALID_REQUEST" => InvalidRequest
            | "UNKNOWN_ERROR" => UnknownError
            | _ => UnknownResponse
        }
    }
}

let createUrl = (apiKey, address) => {
    let query = {
        address.line1
        |> (s) => {switch (address.line2) {
            | Some(line2) => s ++ " " ++ line2 
            | None => s}
        }
        |> (s) => s ++ address.city
        |> (s) => {switch (address.state) {
            | Some(state) => s ++ " " ++ state.abbreviation
            | None => s
            }
        }
        |> (s) => s ++ " " ++ address.postalCode
    };
    geocodingUrl ++ Js.Global.encodeURI(query) ++ "&key=" ++ apiKey
};

let getGeoCoding = (apiKey, address) => {
    let url = createUrl(apiKey, address);
    Js.Promise.(
    Fetch.fetch(url)
        |> then_(Fetch.Response.json)
        |> then_(json => {
            resolve(A19Core.Core.Success(
                {
                    lat: 0.0,
                    lng: 0.0
                }))
        })
        |> catch(error => {
            Js.Console.log(error);
            resolve(A19Core.Core.Error([|"Unexpected error has occurrent while fetching the geo code"|]))
        })
    )
}
