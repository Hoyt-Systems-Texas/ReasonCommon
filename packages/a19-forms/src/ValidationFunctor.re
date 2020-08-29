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
    type t = Validation.stringValidation;
    type value = string;
    type cleaned = option(string);

    let errors = (t: Validation.stringValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateString(t, value);
    let clean = (t: Validation.stringValidation) => t.clean;
    let make = Validation.makeStringValidation;
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
    type t = Validation.postalCodeValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.postalCodeValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validatePostalCode(t, value);
    let clean = (t: Validation.postalCodeValidation) => t.clean;
    let make = Validation.makePostalCodeValidation;

    let makeDefault = (~required=false, ~initialValue=(None: option(string)), ~name, ()) => {
        make(
            ~name,
            ~initialValue,
            ~required=genRequired(name, required)
        )
    }
}

module FloatValidation {
    type t = Validation.floatValidation;
    type value = string;
    type clean = option(float);

    let errors = (t: Validation.floatValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateFloat(t, value);
    let clean = (t: Validation.floatValidation) => t.clean;
    let make = Validation.makeFloatValidation;

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
    type t = Validation.phoneNumberValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.phoneNumberValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validatePhoneNumber(t, value);
    let clean = (t: Validation.phoneNumberValidation) => t.clean;
    let make = Validation.makePhoneNumberValidation;
    let makeDefault = (~required=false, ~initialValue=(None:option(string)), ~name, ()) => {
        make(
            ~name,
            ~required=genRequired(name, required),
            ~initialValue)
    }
}

module IntValidation {
    type t = Validation.intValidation;
    type value = string;
    type clean = option(int);

    let errors = (t: Validation.intValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateInt(t, value);
    let clean = (t: Validation.intValidation) => t.clean;
    let make = Validation.makeIntValidation;
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
    type t = Validation.urlValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.urlValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validationUrl(t, value);
    let clean = (t: Validation.urlValidation) => t.clean;
    let make = Validation.makeUrlValidation;
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
    type t = Validation.emailValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.emailValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validationEmail(t, value);
    let clean = (t: Validation.emailValidation) => t.clean;
    let make = Validation.makeEmailValidation;
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