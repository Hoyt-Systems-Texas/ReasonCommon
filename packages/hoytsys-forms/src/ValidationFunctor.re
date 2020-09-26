module type Validation = {
    /**
     * The type of the validations.
     */
    type t;
    /**
     * The type of the value.
     */
    type value;
    /**
     * The cleaned value of the function.
     */
    type cleaned;

    /**
     * Gest the list of errors.
     */
    let errors: t => list(string);
    let update: t => value => t;
    let clean: t => cleaned;
};

module Make = (Item: Validation) => {

}

let genRequired = (name, required) => {
    if (required) {
        Some(name ++ " is required")
    } else {
        None
    };
}

module StringValidation {
    type t = Validation.StringValidation.t;
    type value = string;
    type cleaned = option(string);

    let errors = (t: Validation.StringValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.StringValidation.validate(t, value);
    let clean = (t: Validation.StringValidation.t) => t.clean;
    let make = Validation.StringValidation.make;
    let makeDefaultFieldSize = (~required=false, ~maxLength=100, ~initialValue=(None: option(string)), ~name: string, ()) => {
        make(
            ~name,
            ~initialValue,
            ~required=genRequired(name, required),
            ~minLength=0,
            ~maxLength
        )
    }
};

module PostalCodeValidation {
    type t = Validation.PostalCodeValidation.t;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.PostalCodeValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.PostalCodeValidation.validate(t, value);
    let clean = (t: Validation.PostalCodeValidation.t) => t.clean;
    let make = Validation.PostalCodeValidation.make;

    let makeDefault = (~required=false, ~initialValue=(None: option(string)), ~name, ()) => {
        make(
            ~name,
            ~initialValue,
            ~required=genRequired(name, required)
        )
    }
}

module FloatValidation {
    type t = Validation.FloatValidation.t;
    type value = string;
    type clean = option(float);

    let errors = (t: Validation.FloatValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.FloatValidation.validate(t, value);
    let clean = (t: Validation.FloatValidation.t) => t.clean;
    let make = Validation.FloatValidation.make;

    let makeDefaultGeo = (~required=false, ~initialValue=(None:option(float)), ~name, ()) => {
        make(
            ~name,
            ~required=genRequired(name, required),
            ~initialValue,
            ~minValue=-180.0,
            ~maxValue=180.0
        )
    }
}

module PhoneNumberValidation {
    type t = Validation.PhoneNumberValidation.t;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.PhoneNumberValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.PhoneNumberValidation.validate(t, value);
    let fixValue = (t, value) => {
        let digits = HoytsysCore.Core.removeNonDigits(value);
        if (Js.String.length(digits) == 10) {
            update(t, "1" ++ digits);
        } else {
            update (t, digits);
        }
    };

    let clean = (t: Validation.PhoneNumberValidation.t) => t.clean;
    let make = Validation.PhoneNumberValidation.make;
    let makeDefault = (~required=false, ~initialValue=(None:option(string)), ~name, ()) => {
        make(
            ~name,
            ~required=genRequired(name, required),
            ~initialValue)
    }
}

module IntValidation {
    type t = Validation.IntValidation.t;
    type value = string;
    type clean = option(int);

    let errors = (t: Validation.IntValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.IntValidation.validate(t, value);
    let clean = (t: Validation.IntValidation.t) => t.clean;
    let make = Validation.IntValidation.make;
    let makeDefaultPos = (~required=false, ~initialValue=(None:option(int)), ~maxValue=100_000_000, ~name, ()) => {
        make(
            ~name,
            ~required=genRequired(name, required),
            ~initialValue,
            ~max=maxValue,
            ~min=0
        )
    }
}

module UrlValidation {
    type t = Validation.UrlValidation.t;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.UrlValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.UrlValidation.validate(t, value);
    let clean = (t: Validation.UrlValidation.t) => t.clean;
    let make = Validation.UrlValidation.make;
    let makeDefault = (~required=false, ~initialValue=(None:option(string)), ~maxLength=255, ~name, ()) => {
        make(
            ~name,
            ~initialValue,
            ~required=genRequired(name, required),
            ~minLength=2,
            ~maxLength,
        )
    }
}

module EmailValidation {
    type t = Validation.EmailValidation.t;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.EmailValidation.t) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.EmailValidation.validate(t, value);
    let clean = (t: Validation.EmailValidation.t) => t.clean;
    let make = Validation.EmailValidation.make;
    let makeDefault = (~required=false, ~initialValue=(None:option(string)), ~maxLength=255, ~name, ()) => {
        make(
            ~name,
            ~initialValue,
            ~required=genRequired(name, required),
            ~minLength=0,
            ~maxLength
        )
    }
}

module EnumerateValidation {
    type t('a) = Validation.EnumerateValueValidation.t('a);

    let errors(t: t('a)) = t.validation.errors;
    let update(t, value) = Validation.EnumerateValueValidation.validate(t, value);
    let clean(t: t('a)) = t.value;
    let make = Validation.EnumerateValueValidation.make;
}