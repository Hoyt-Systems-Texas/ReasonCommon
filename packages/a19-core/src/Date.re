module Time {

    module Timezone {

        external tz : MomentRe.Moment.t => string => MomentRe.Moment.t = "tz";

        external withTz: string => string => MomentRe.Moment.t = "tz";
    };

    let formatTime = (number: int) => {
        if (number < 10) {
            "0" ++ string_of_int(number)
        } else {
            string_of_int(number)
        }
    };

    let toTzKeepTime = (date, timezone) => {
        let hour = string_of_int(MomentRe.Moment.hour(date));
        let minutes = formatTime(MomentRe.Moment.minute(date));
        let seconds = formatTime(MomentRe.Moment.second(date));
        Timezone.withTz(
            "1970-1-1 " ++
             hour ++ ":" ++ 
             minutes ++ ":" ++ 
             seconds, timezone);
    }

    let toMinsFromEpoc = (date) => {
        MomentRe.Moment.toUnix(date) / 60
    }
}