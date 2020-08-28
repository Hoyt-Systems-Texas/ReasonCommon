/**
 * A fast matching system that normalizes the data.
 * Able to provided different ways of matching.  Need to be able to have realy fast matching.
 * Take in a data structure and output the result.  Can be represented as a functor.
 */
module type BasicMatchInfo = {
    type collection;
    
    /**
     * The type of the internal values.
     */
    type record;

    /**
     * The type of the unkown record you are trying to match.
     */
    type unknownRecord;

    /**
     * Converts the collection to a list.
     */
    let asList: collection => list(record);
};

module MakeBasicBatch = (Item: BasicMatchInfo) => {

    /**
     * The different type of matchin ghte system surppots.
     */
    type matchingBuilder = 
        | HashStringMatch(Item.record => option(string), Item.unknownRecord => option(string))
        | WordMatch(Item.record => option(string), Item.unknownRecord => option(string))
        ;

    type matchingBuilderRecord = list(matchingBuilder);
    
    type matchingBuilderOneOf = list(matchingBuilderRecord);

    type matchTryEntry =
        | Hash(Belt.HashMap.String.t(matchTryEntry), Item.unknownRecord => option(string))
        | Word(list((list(string), matchTryEntry)), Item.unknownRecord => option(string))
        | Found(list(Item.record))
        ;

    type matchTryOneOf = list(matchTryEntry);

    let buildMatching = (oneOf: matchingBuilderOneOf, collection: Item.collection): matchTryOneOf => {
        Belt.List.map(oneOf, (pMatch) => {
            let rec buildMatch = (match, records) => {
                switch (match) {
                    | [] => {
                        Found(records)
                    }
                    | [match, ...rest] => {
                        switch (match) {
                            | HashStringMatch(oldRecord, newRecord) => {
                                let map = Belt.List.reduce(
                                    records,
                                    Belt.HashMap.String.make(~hintSize=1000),
                                    (map, record) => {
                                        switch (oldRecord(record)) {
                                            | Some(value) => {
                                                let current = switch (Belt.HashMap.String.get(map, value)) {
                                                    | Some(theList) => {
                                                        theList
                                                    }
                                                    | None => {
                                                        []
                                                    }
                                                }
                                                Belt.HashMap.String.set(
                                                    map, 
                                                    Js.String.toLocaleLowerCase(value),
                                                    [record, ...current]);
                                                map
                                            }
                                            | None => map
                                        }
                                    });
                                let lookup = Belt.HashMap.String.reduce(
                                    map,
                                    Belt.HashMap.String.make(~hintSize=1000),
                                    (map, key, value) => {
                                        Belt.HashMap.String.set(map, key, buildMatch(rest, value));
                                        map
                                    })
                                Hash(lookup, newRecord)
                            }
                            | WordMatch(oldRecord, newRecord) => {
                                let rec updatePotentialMatches = (matches, findValue, record) => {
                                    switch (matches) {
                                        | [] => [(findValue, [record])]
                                        | [(testValue, records), ...rest] => {
                                            if (testValue == findValue) {
                                                [(testValue, [record, ...records]), ...rest]
                                            } else {
                                                [(testValue, records)] @ updatePotentialMatches(
                                                    rest,
                                                    findValue,
                                                    record);
                                            }
                                        }
                                    }
                                }
                                let value = Belt.List.reduce(records, [], (matches, record) => {
                                    switch (oldRecord(record)) {
                                        | Some(value) => {
                                            let tokenized = A19Core.Core.tokenizeString(value);
                                            updatePotentialMatches(matches, tokenized, record);

                                        }
                                        | None => matches
                                    }
                                });
                                let matching = Belt.List.map(value, ((words, records)) => {
                                    (words, buildMatch(rest, records));
                                });
                                Word(matching, newRecord)
                            }
                        }
                    }
                }
            }
            buildMatch(pMatch, Item.asList(collection))
        })
    }

    let rec findMatchEntry = (match: matchTryEntry, unknown: Item.unknownRecord) => {
        switch (match) {
            | Hash(map, getValue) => {
                switch (getValue(unknown)) {
                    | Some(value) => {
                        switch (Belt.HashMap.String.get(map, value)) {
                            | Some(entry) => findMatchEntry(entry, unknown)
                            | None => []
                        }
                    }
                    | None => []
                }
            }
            | Word(words, getValue) => {
                switch (getValue(unknown)) {
                    | Some(value) => {
                        let tokens = A19Core.Core.tokenizeString(value);
                        switch (tokens) {
                            | [] => []
                            | matchWords => {
                                Belt.List.reduce(words, [], (match, (words, matchEntry)) => {
                                    if (matchWords == words) {
                                        match @ findMatchEntry(matchEntry, unknown)
                                    } else {
                                        match
                                    }
                                })
                            }
                        }
                    }
                    | None => []
                }
            }
            | Found(records) => {
                records
            }
        }
    }

    let findMatchFirst = (oneOf: matchTryOneOf, unknown: Item.unknownRecord): list(Item.record) => {
        let rec findFirst = (matchEntries) => {
            switch (matchEntries) {
                | [] => []
                | [matchEntry, ...rest] => {
                    switch (findMatchEntry(matchEntry, unknown)) {
                        | [] => findFirst(rest)
                        | match => match
                    }
                }
            }
        };
        findFirst(oneOf);
    }
};