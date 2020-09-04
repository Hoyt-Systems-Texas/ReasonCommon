open Jest;

describe("radians test", () => {
    open Expect;
    open A19Core.Math;  

    test("To radians test", () => {
        let value = toDegress(1.0);
        expect(value) |> toBeCloseTo(57.2958);
    })
});