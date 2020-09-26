open Jest;

let googleApiKey = "AIzaSyCB1zYbkydr_QecDgZ18XPSktSLapbgHd4";

[%%raw "var fetch = require('whatwg-fetch'); global.fetch = fetch;"]

describe("Fectch google check.", () => {
    open Expect;
    open GeoCoding;
    open HoytsysCore.Model.Address;
    open HoytsysCore.Model.State;

    testPromise("Check geo coding of an address", () => {
        Js.Promise.(
            getGeoCoding(googleApiKey, {
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
            })
            |> then_(result => {
                switch (result) {
                    | HoytsysCore.Core.Success(g) => {
                        Js.Console.log(g);
                        resolve(expect(true) |> toBe(true))
                    }
                    | _ => {
                        resolve(failwith("unexpected result."))
                    }
                }
            })
        );
    });
})