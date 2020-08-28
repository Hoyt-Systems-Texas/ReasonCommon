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

module StringValidation {
    type t = Validation.stringValidation;
    type value = string;
    type cleaned = option(string);

    let errors = (t: Validation.stringValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateString(t, value);
    let clean = (t: Validation.stringValidation) => t.clean;
}

module PostalCodeValidation {
    type t = Validation.postalCodeValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.postalCodeValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validatePostalCode(t, value);
    let clean = (t: Validation.postalCodeValidation) => t.clean;
}

module FloatValidation {
    type t = Validation.floatValidation;
    type value = string;
    type clean = option(float);

    let errors = (t: Validation.floatValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateFloat(t, value);
    let cean = (t: Validation.floatValidation) => t.clean;
}

module PhoneNumberValidation {
    type t = Validation.phoneNumberValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.phoneNumberValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validatePhoneNumber(t, value);
    let clean = (t: Validation.phoneNumberValidation) => t.clean;
}

module IntValidation {
    type t = Validation.intValidation;
    type value = string;
    type clean = option(int);

    let errors = (t: Validation.intValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validateInt(t, value);
    let clean = (t: Validation.intValidation) => t.clean;
}

module UrlValidation {
    type t = Validation.urlValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.urlValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validationUrl(t, value);
    let clean = (t: Validation.urlValidation) => t.clean;
}

module EmailValidation {
    type t = Validation.emailValidation;
    type value = string;
    type clean = option(string);

    let errors = (t: Validation.emailValidation) => t.baseValidation.validation.errors;
    let update = (t, value) => Validation.validationEmail(t, value);
    let clean = (t: Validation.emailValidation) => t.clean;
}