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

module Convert {

    let toMiles = (meters) => {
        meters *. 0.000621371
    }
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

    module Haversine {

        let earthRadiusMeters = 6_371_000.0
        let calculate = (p1, p2) => {
            let phi1 = Math.toRadians(p1.lat);
            let phi2 = Math.toRadians(p2.lat);
            let changePhi = Math.toRadians((p2.lat -. p1.lat));
            let changeLambda = Math.toRadians((p2.lng -. p1.lng));
            let a = Js.Math.sin(changePhi /. 2.0) *. Js.Math.sin(changePhi /. 2.0) +.
                Js.Math.cos(phi1) *. Js.Math.cos(phi2) *.
                Js.Math.sin(changeLambda /. 2.0) *. Js.Math.sin(changeLambda /. 2.0);
            let c = 2.0 *. Js.Math.atan2(~y=Js.Math.sqrt(a), ~x=Js.Math.sqrt(1.0 -.a), ())
            earthRadiusMeters *. c
        };
    }

    module Destination {
        let calculate = (start, bearing, distance) => {
            /**
            * const φ2 = Math.asin( Math.sin(φ1)*Math.cos(d/R) +
            * Math.cos(φ1)*Math.sin(d/R)*Math.cos(brng) );
            * const λ2 = λ1 + Math.atan2(Math.sin(brng)*Math.sin(d/R)*Math.cos(φ1),
            * Math.cos(d/R)-Math.sin(φ1)*Math.sin(φ2));k
             */
            let phi1 = Math.toRadians(start.lat);
            let lmd1 = Math.toRadians(start.lng);
            let bearing = Math.toRadians(bearing);
            let distanceHov = distance /. Haversine.earthRadiusMeters;
            let phi2 = asin(
                sin(phi1) *. cos(distanceHov) +.
                cos(phi1) *. sin(distanceHov) *. cos(bearing));
            let lmd2 = lmd1 +. atan2(sin(bearing) *. sin(distanceHov) *. cos(phi1), 
                             cos(distanceHov) -. sin(phi1) *. sin(phi2));
            {
                lat: (Math.toDegress(phi2)),
                lng: (Math.toDegress(lmd2))
            }
        }
    }
}