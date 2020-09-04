open Jest;

describe("Haversine test", () => {
    open Expect;
    open A19Core.Core;

    test("calculate test", () => {
        let dist =  Model.Location.Haversine.calculate({
            lat: 20.0,
            lng: 20.0
        }, {
            lat: 22.0,
            lng: 22.0
        });
       expect(dist) |> toBeCloseTo(304_230.084)
    })
})