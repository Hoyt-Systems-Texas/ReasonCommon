open A19Core.Model.Location;

module GeoLocation {
    type geoLocationSuccess;
    type geoPosition;
    type geoLocationError;
    type geoLocation;

    [@bs.val] external geoLocation: geoLocation = "navigator.geolocation";

    [@bs.send]
    external getLocation: geoLocation => (geoLocationSuccess => unit)  => (geoLocationError => unit) => unit = "getCurrentPosition";

    [@bs.get]
    external get_pos: geoLocationSuccess => geoPosition = "coords";

    [@bs.get]
    external get_lat: geoPosition => float = "latitude";

    [@bs.get]
    external get_lng: geoPosition => float = "longitude";

    let fromGeoLocation = (geoLocationSuccess) => {
        let coords = get_pos(geoLocationSuccess);
        {
            lat: get_lat(coords),
            lng: get_lng(coords)
        }
    }
};

let getLocation = () => {
    Js.Promise.make((~resolve, ~reject) => {
        GeoLocation.getLocation(
            GeoLocation.geoLocation,
             success => {
            resolve(. A19Core.Core.Success(GeoLocation.fromGeoLocation(success)))
        }, _ => {
            resolve(. A19Core.Core.Error([|"Unable to get the location"|]))
        });
    });
};
