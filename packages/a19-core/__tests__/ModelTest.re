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
    });
});

describe("Destination test", () => {
    open Expect;
    open A19Core.Core;

    let dist = Model.Location.Destination.calculate({
        lat: 20.0,
        lng: 20.0,
    }, 0.0, 2000.0);
    test("calculate lat test", () => {
        expect(dist.lat) |> toBeCloseTo(20.0179)
    });
    test("calculate lng test", () => {
        expect(dist.lng) |> toBeCloseTo(20.0);
    })
})