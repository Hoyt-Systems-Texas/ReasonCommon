module Time {

    [@bs.new][@bs.module("moment-timezone")]
    external ignore_me: unit => unit = "aaa";

    let makeIgnore = () => {
        ignore_me();
    }

    module Timezone {

        [@bs.send]
        external tz : MomentRe.Moment.t => string => MomentRe.Moment.t = "tz";

        [@bs.module("moment")]
        external withTz: string => string => MomentRe.Moment.t = "tz";

        [@bs.send]
        external utc: MomentRe.Moment.t => MomentRe.Moment.t = "utc";

        [@bs.send]
        external utcFrom: string => MomentRe.Moment.t = "utc";
    };

    let formatTime = (number: int) => {
        if (number < 10) {
            "0" ++ string_of_int(number)
        } else {
            string_of_int(number)
        }
    };

    let toTzKeepTime = (date, timezone) => {
        let hour = formatTime(MomentRe.Moment.hour(date));
        let minutes = formatTime(MomentRe.Moment.minute(date));
        let seconds = formatTime(MomentRe.Moment.second(date));
        let time = "1970-01-01 " ++
             hour ++ ":" ++ 
             minutes ++ ":" ++ 
             seconds;
        Js.Console.log3(hour, minutes, seconds);
        let time = Timezone.withTz(
            time, timezone);
        time
    }

    let toMinsFromEpoc = (date) => {
        let unixTime = date
        -> MomentRe.Moment.clone
        -> Timezone.tz("UTC")
        |> MomentRe.Moment.toUnix;
        unixTime / 60
    }

    let fromUnixEpoc = (mins) => {
        MomentRe.momentWithUnix(mins * 60)
    }
}

module DateHelpers {

    let isoFormat = "YYYY-MM-DD";
    let usFormat = "MM/DD/YYYY";

    let toString (date) = {
        MomentRe.Moment.format(isoFormat, date)
    }

    let toStringUs(date) = {
        MomentRe.Moment.format(usFormat, date)
    }

    let toStringOption(date) = {
        Core.OptionExt.map(date, toString)
    }

    let fromString(date) = {
        try(Some(MomentRe.momentWithFormat(date, usFormat))) {
            | _ => None
        }
    }
}
