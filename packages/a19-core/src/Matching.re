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
     * Need to build the data structure.
     */

    let reduce: collection => 'a => (('a, record) => 'a) => 'a;
};
module MakeBasicBatch = (Item: BasicMatchInfo) => {

    /**
     * The different type of matchin ghte system surppots.
     */
    type matchingBuilder = 
        | HashStringMatch(Item.record => string, Item.unknownRecord => option(string))
        | WordMatch(Item.record => string, Item.unknownRecord => option(string))
        ;

    type matchingBuilderRecord = list(matchingBuilder);
    
    type matchingBuilderOneOf = list(matchingBuilderRecord);

    type matchTryEntry =
        | Hash(Belt.HashMap.String.t(matchTryEntry), Item.unknownRecord => option(string))
        | Word(list((list(string), matchTryEntry)), Item.unknownRecord => option(string))
        | Found(Item.record)
        ;

    type matchTries = list(matchTryEntry);

    type matchTryOneOf = list(matchTries);

    let buildMatching = (oneOf: matchingBuilderOneOf, collection: Item.collection): matchTryOneOf => {
        []
    }

    let findMatch = (oneOf: matchTryOneOf, unknown: Item.unknownRecord): option(Item.record) => {
        None
    }
};

module type Comparable = {
  type t;
  let equal: (t, t) => bool;
};

module MakeSet = (Item: Comparable) => {
  /* let's use a list as our naive backing data structure */
  type backingType = list(Item.t);
  let empty = [];
  let add = (currentSet: backingType, newItem: Item.t) : backingType =>
    /* if item exists */
    if (List.exists((x) => Item.equal(x, newItem), currentSet)) {
      currentSet /* return the same (immutable) set (a list really) */
    } else {
      [
        newItem,
        ...currentSet /* prepend to the set and return it */
      ]
    };
};