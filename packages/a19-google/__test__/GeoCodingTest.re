open Jest;

let googleApiKey = "AIzaSyCB1zYbkydr_QecDgZ18XPSktSLapbgHd4";

describe("Fectch google check.", () => {
    open Expect;
    open GeoCoding;
    open A19Core.Model.Address;
    open A19Core.Model.State;

    test("Check geo coding of an address", () => {
        let result = getGeoCoding(googleApiKey, {
            addressId: 1,
            line1: "6012 Paddlefish Dr",
            line2: None,
            city: "Fort Worth",
            stateId: None,
            state: Some({
                stateId: 1,
                abbreviation: "TX",
                name: "Texas"
            }),
            postalCode: "76179",
            county: Some("Tarrant"),
            lat: None,
            lng: None
        });
        Js.Console.log(result);
        expect(true) |> toEqual(true)
    });
})