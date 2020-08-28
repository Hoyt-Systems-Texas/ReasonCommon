open Jest;

type testType = {
    v1: string,
    v2: string,
    v3: string,
}

let testTypes = [
    {
        v1: "one",
        v2: "two",
        v3: "three",
    },
    {
        v1: "one one",
        v2: "two two",
        v3: "three three three",
    },
];

module TestTypeMatchInfo = {
    type collection = list(testType);
    type record = testType;
    type unknownRecord = testType;
    let reduce = Belt.List.reduce;
    let asList = a => a;
}

module TestTypeMatch = Matching.MakeBasicBatch(TestTypeMatchInfo);

describe("Basic search match.", () => {
    open Expect;
    let matches = [
        [
            TestTypeMatch.HashStringMatch( r => Some(r.v1), r => Some(r.v1)),
            TestTypeMatch.WordMatch(r => Some(r.v2), r => Some(r.v2))
        ],
        [
            TestTypeMatch.HashStringMatch(r => Some(r.v3), r => Some(r.v3))
        ]
    ];
    let oneOf = TestTypeMatch.buildMatching(matches, testTypes);
    test("Test match hash and one word", () => {
        let testRec = {v1: "one", v2: "two", v3: "somthingElse"};
        let checkValue = {...testRec, v3: "three"};
        let matches = TestTypeMatch.findMatchFirst(oneOf, testRec);
        expect(matches) |> toEqual([checkValue])
    });

    test("Test no match check", () => {
        let testRec = {v1: "one", v2: "Something Else", v3: ""};
        let matches = TestTypeMatch.findMatchFirst(oneOf, testRec);
        expect(matches) |> toEqual([]);
    });
    test("Test multi words", () => {
        let testRec = {v1: "one one", v2: "two two", v3: "three three three"};
        let matches = TestTypeMatch.findMatchFirst(oneOf, testRec);
        expect(matches) |> toEqual([testRec]);
    });
    test("Test multi one", () => {
        let testRec = {v1: "so", v2: "el", v3: "three"};
        let matches = TestTypeMatch.findMatchFirst(oneOf, testRec);
        expect(matches) |> toEqual([{
            ...testRec,
            v1: "one",
            v2: "two"
        }]);
    })
})