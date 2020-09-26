open Jest;

describe("Clean number test", () => {
    open Expect;
    open HoytsysCore.Core;

    test("Test remove digits", () => {
        let result = removeNonDigits(" 1+2++3+4#5");
        expect(result) |> toEqual("12345");
    });

    test("Test remove digits 2", () => {
        let result = removeNonDigits(" 1+2ac3+4#5");
        expect(result) |> toEqual("12345");
    });

});