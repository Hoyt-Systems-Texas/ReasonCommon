module State {
    type state = {
        stateId: int,
        name: string,
        abbreviation: string
    };

    type stateNameLookup = Belt.HashMap.Int.t(state);

    let toStateNameLookup = (states) => {
        Belt.Array.reduce(states, Belt.HashMap.String.make(~hintSize=100), (map, state) => {
            Belt.HashMap.String.set(map, Js.String.toLocaleLowerCase(state.name), state);
            Belt.HashMap.String.set(map, Js.String.toLocaleLowerCase(state.abbreviation), state);
            map
        })
    };

    let getState = (lookup, name) => {
        let name = Js.String.trim(name);
        Belt.HashMap.String.get(lookup, Js.String.toLocaleLowerCase(name))
    };

}

module Address {
    type address = {
        addressId: int,
        line1: string,
        line2: option(string),
        city: string,
        stateId: option(int),
        state: option(State.state),
        postalCode: string,
        county: option(string),
        lat: option(float),
        lng: option(float),
    };
}

module Location {
    type geoCode = {
        lat: float,
        lng: float,
    }

    // A good starting place for the US.
    let kansas = {lat: 38.4773979, lng: -100.5624572};

    module LatLngBounds {
        type t = {
            east: geoCode,
            north: geoCode,
            south: geoCode,
            west: geoCode
        };
    }

}