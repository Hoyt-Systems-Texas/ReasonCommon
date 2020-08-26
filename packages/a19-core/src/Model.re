module State {
    type state = {
        stateId: int,
        name: string,
        abbreviation: string
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
}

