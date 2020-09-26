let usZipCode = [%re "/\d{5}/g"];

/**
 * Checks to see if a value is postal code.
 */
let isPostalCode = (postalCode) => {
    Js.String.match(Validation.PostalCodeValidation.usZipCode, postalCode)
}