type validation = {
    errors: list(string),
    dirty: bool,
};

type baseValidation = {
    validation,
    value: string,
    required: option(string),
    name: string,
}

type stringValidation = {
    baseValidation,
    clean: option(string),
    maxLength: int,
    minLength: int,
}

let validateString = (validation: stringValidation, value: string) => {
    let dirty = true;
    let clean = String.trim(value);
    let (clean, errors) = {
        let length = String.length(clean);
        if (length > 0) {
            let errors = if (length < validation.minLength) {
                [validation.baseValidation.name ++ " must be at least " ++ string_of_int(validation.minLength)]
            } else {
                []
            };
            let errors = if (length > validation.maxLength) {
                List.cons(validation.baseValidation.name ++ " must be smaller than " ++ string_of_int(validation.maxLength), errors)
            } else {
                errors
            };
            if (Belt.List.length(errors) > 0) {
                (None, errors)
            } else {
                (Some(clean), [])
            }
        } else {
            switch (validation.baseValidation.required) {
                | Some(value) => (None, [value])
                | None => (None, [])
            };
        }
    };
    {
        ...validation,
        clean: clean,
        baseValidation: {
            ...validation.baseValidation,
            value,
            validation: {
                errors,
                dirty
            }
        }
    }
}

let optionString_to_string = (value: option(string)) => {
    switch (value) {
        | Some(v) => v
        | None => ""
    }
}

let makeStringValidation = (~name, ~initialValue, ~required, ~minLength, ~maxLength): stringValidation => {
    let valid = {
        baseValidation: {
            validation: {
                errors: [],
                dirty: false
            },
            name,
            required,
            value: switch (initialValue) {
            | Some(v) => v
            | None => ""
            }
        },
        clean: initialValue,
        minLength,
        maxLength
    };
    let valid = validateString(valid, valid.baseValidation.value);
    {
        ...valid,
        baseValidation: {
            ...valid.baseValidation,
            validation: {
                ...valid.baseValidation.validation,
                dirty: false
            }
        }
    }
}

type postalCodeValidation = {
    baseValidation,
    clean: option(string),
}

let usZipCode = [%re "/\d{5}/g"];

let validatePostalCode = (validation: postalCodeValidation, value: string) => {
    let dirty = true;
    let clean = String.trim(value);
    let (clean, errors) = {
        let length = String.length(clean);
        if (length > 0) {
            let errors = if (length != 5) {
                [validation.baseValidation.name ++ " postal code must be 5 digits"]
            } else {
                []
            };
            let errors = if (Js.String.match(usZipCode, clean) == None) {
                List.cons(validation.baseValidation.name ++ " invalid zip code.", errors)
            } else {
                []
            };
            if (Belt.List.length(errors) > 0) {
                (None, errors)
            } else {
                (Some(clean), [])
            }
        } else {
            switch (validation.baseValidation.required) {
            | Some(v) => (None, [v])
            | None => (None, [])
            }
        }
    };
    {
        clean,
        baseValidation: {
            ...validation.baseValidation,
            value,
            validation: {
                errors,
                dirty
            }
        }
    }
}

let makePostalCodeValidation = (~name, ~initialValue, ~required) => {
    let valid = {
        baseValidation: {
            validation: {
                errors: [],
                dirty: false
            },
            name,
            required,
            value: ""
        },
        clean: initialValue
    };
    let valid = validatePostalCode(valid, valid.baseValidation.value);
    {
        ...valid,
        baseValidation: {
            ...valid.baseValidation,
            validation: {
                ...valid.baseValidation.validation,
                dirty: false
            }
        }
    }
}

type enumerateValueValidation('a) = {
    validation,
    required: option(string),
    value: option('a)
}

let makeEnumerateValidation = (~required, ~initialValue: option('a)) => {
    {
        validation: {
            errors: [],
            dirty: false
        },
        required,
        value: initialValue
    }
};

let validateEnumerate = (validation, value: option('a)) => {
    let dirty = true;
    let (errors) = {
        switch (validation.required) {
        | Some(e) => {
            if (value == None) {
                [e]
            } else {
                []
            }
        }
        | None => []
        }
    };
    {
        ...validation,
        validation: {
            errors,
            dirty
        },
        value: value
    }
}

type floatValidation = {
    baseValidation,
    clean: option(float),
    minValue: float,
    maxValue: float,
}

let optionFloat_to_string = (value: option(float)) => {
    switch (value) {
    | Some(value) => Js.Float.toString(value)
    | None => ""
    }
}

let validateFloat = (validation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) = if (String.length(clean) > 0) {
        switch (float_of_string_opt(clean)) {
            | Some(clean) => {
                let errors = if (clean < validation.minValue) {
                    [validation.baseValidation.name ++ " must be larger than " ++ Js.Float.toString(validation.minValue)]
                } else {
                    []
                };
                let errors = if (clean > validation.maxValue) {
                    List.cons(validation.baseValidation.name ++ " must be smaller than " ++ Js.Float.toString(validation.maxValue), errors)
                } else {
                    errors
                };
                if (Belt.List.length(errors) > 0) {
                    (errors, None)
                } else {
                    ([], Some(clean))
                }
            }
            | None => {
                ([validation.baseValidation.name ++ " invalid numeric value."], None)
            }    
        }
    } else {
        switch (validation.baseValidation.required) {
        | Some(e) => ([e], None)
        | None => ([], None)
        }
    };
    {
        ...validation,
        clean,
        baseValidation: {
            ...validation.baseValidation,
            value,
            validation: {
                dirty,
                errors
            }
        }
    }
}

let makeFloatValidation = (~name, ~required, ~initialValue, ~minValue, ~maxValue): floatValidation => {
    let valid = {
        baseValidation: {
            name,
            validation: {
                    dirty: false,
                    errors: []
                },
            value: "",
            required
        },
        clean: initialValue,
        minValue: minValue,
        maxValue: maxValue,
    };
    validateFloat(valid, valid.baseValidation.value)
}

let hasErrors = (validation: array(validation)) => {
    Belt.Array.length(Belt.Array.keep(validation, v => Belt.List.length(v.errors) > 0)) > 0
}

type phoneNumberValidation = {
    baseValidation,
    clean: option(string)
}
let allDigits = [%re "/\d+/g"];

let validatePhoneNumber = (validation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) = if (String.length(clean) > 0) {
        if (String.length(clean) > 10 && String.length(clean) <= 20) {
            // Make sure they are all digits
            if (Js.String.match(allDigits, clean) == None) {
                (["Phone number is invalid."], None)
            } else {
                ([], Some(clean))
            }
        } else {
            (["Phone number not log enough."], None)
        }
    } else {
        switch (validation.baseValidation.required) {
            | Some(e) => ([e], None)
            | None => ([], None)
        }
    };
    {
        baseValidation: {
            ...validation.baseValidation,
            validation: {
                dirty,
                errors
            },
            value
        },
        clean
    }
}

let makePhoneNumberValidation = (~name, ~required, ~initialValue) => {
    let valid = {
        baseValidation: {
            name,
            validation: {
                dirty: false,
                errors: [],
            },
            value: "",
            required,
        },
        clean: initialValue
    }
    let valid = validatePhoneNumber(valid, valid.baseValidation.value);
    {
        ...valid,
        baseValidation: {
            ...valid.baseValidation,
            validation: {
                ...valid.baseValidation.validation,
                dirty: false
            }
        }
    }
}

type intValidation = {
    baseValidation,
    min: int,
    max: int,
    clean: option(int),
};

let optionInt_to_string = (value: option(int)) => {
    switch (value) {
    | Some(i) => string_of_int(i)
    | None => ""
    }
}

let validateInt = (validation: intValidation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) = if (String.length(clean) > 0) {
        switch (int_of_string_opt(clean)) {
            | Some(value) => {
                let errors = []
                |> (errors) => {if (value < validation.min) {
                        List.cons(validation.baseValidation.name ++ " must be less than " ++ string_of_int(validation.min), errors)
                    } else {
                        []
                    }
                }
                |> (errors) => {
                    if (value > validation.max) {
                        List.cons(validation.baseValidation.name ++ " must be less than " ++
                            string_of_int(validation.max), errors)
                    } else {
                        []
                    }
                };
                if (List.length(errors) > 0) {
                    (errors, Some(value))
                }  else {
                    ([], Some(value))
                }
            } 
            | None => {
                (["Not a number."], None)
            }
        }
    } else {
        switch (validation.baseValidation.required) {
            | Some(e) => ([e], None)
            | None => ([], None)
        }
    };
    {
        ...validation,
        baseValidation: {
            ...validation.baseValidation,
            validation: {
                dirty,
                errors
            },
            value,
        },
        clean: clean,
    }
}

let makeIntValidation = (~name, ~required, ~initialValue, ~min, ~max) => {
    {
        baseValidation: {
            name,
            validation: {
                dirty: false,
                errors: []
            },
            value: switch (initialValue) {
                | Some(e) => string_of_int(e)
                | None => ""
            },
            required,
        },
        clean: initialValue,
        min,
        max
    }
}

type urlValidation = stringValidation;

let makeUrlValidation = (~name, ~initialValue, ~required, ~minLength, ~maxLength) => {
    let valid = {
        baseValidation: {
            validation: {
                errors: [],
                dirty: false,
            },
            name,
            required,
            value: switch(initialValue) {
                | Some(v) => v
                | None => ""
            }
        },
        clean: initialValue,
        maxLength,
        minLength
    };

    valid
}

let validUrl: string => bool = [%bs.raw {|
    function(url) {
        try {
            new URL(url);
            return true;
        } catch {
            return false
        }
    } 
|}]

let validationUrl = (validation, value) => {
    let valid = validateString(validation, value);
    let (errors, clean) = switch (valid.clean) {
        | Some(url) => {
            if (validUrl(url)) {
                (valid.baseValidation.validation.errors, None)
            } else {
                (List.cons(valid.baseValidation.name ++ " is invalid.",
                    valid.baseValidation.validation.errors), valid.clean)
            }
        }
        | None => {
            (valid.baseValidation.validation.errors, valid.clean)
        }
    };
    {
        ...valid,
        baseValidation: {
            ...valid.baseValidation, 
            validation: {
                ...valid.baseValidation.validation,
                errors
            }
        },
        clean
    }
}

type emailValidation = stringValidation;

let makeEmailValidation = (~name, ~initialValue, ~required, ~minLength, ~maxLength) => {
    let valid = {
        baseValidation: {
            validation: {
                errors: [],
                dirty: false
            },
            name,
            required,
            value: switch(initialValue) {
                | Some(v) => v
                | None => ""
            },
        },
        clean: initialValue,
        maxLength,
        minLength
    };
    valid 
}

let emailRegex = [%re "/[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/g"];

let validationEmail = (validation, value) => {
    let valid = validateString(validation, value);
    let (errors, clean) = switch (valid.clean) {
        | Some(v) => {
            if (Js.String.match(emailRegex, v) == None) {
                (valid.baseValidation.validation.errors
                    |> List.cons("Invalid email address."), None)
            } else {
                (valid.baseValidation.validation.errors, valid.clean)
            }
        }
        | None => (valid.baseValidation.validation.errors, valid.clean)
    };
    {
        ...valid,
        baseValidation: {
            ...valid.baseValidation, 
            validation: {
                ...valid.baseValidation.validation,
                errors
            }
        },
        clean
    }
}
