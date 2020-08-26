open A19Core.Model.Address;
open A19Core.Model.Location;

let getGeoCoding : string => address => Js.Promise.t(A19Core.Core.resultMonad(geoCode));

type geoCodingResult;
type geoCodeOk;