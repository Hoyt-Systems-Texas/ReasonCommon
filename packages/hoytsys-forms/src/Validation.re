open HoytsysCore.Date;

type validation = {
  errors: list(string),
  dirty: bool,
};

type baseValidation = {
  validation,
  value: string,
  required: option(string),
  name: string,
};

let hasErrors = errors => {
  Belt.Array.reduce(errors, false, (b, e) => {b || Belt.List.length(e) > 0});
};

module StringValidation = {
  type t = {
    baseValidation,
    clean: option(string),
    maxLength: int,
    minLength: int,
  };

  let errors(t)= {
    t.baseValidation.validation.errors
  }

  let value(t) = {
    t.baseValidation.value
  }

  let validation(t) = {
    t.baseValidation.validation
  }

  let validate = (validation: t, value: string) => {
    let dirty = true;
    let clean = String.trim(value);
    let (clean, errors) = {
      let length = String.length(clean);
      if (length > 0) {
        let errors =
          if (length < validation.minLength) {
            [
              validation.baseValidation.name
              ++ " must be at least "
              ++ string_of_int(validation.minLength),
            ];
          } else {
            [];
          };
        let errors =
          if (length > validation.maxLength) {
            List.cons(
              validation.baseValidation.name
              ++ " must be smaller than "
              ++ string_of_int(validation.maxLength),
              errors,
            );
          } else {
            errors;
          };
        if (Belt.List.length(errors) > 0) {
          (None, errors);
        } else {
          (Some(clean), []);
        };
      } else {
        switch (validation.baseValidation.required) {
        | Some(value) => (None, [value])
        | None => (None, [])
        };
      };
    };
    {
      ...validation,
      clean,
      baseValidation: {
        ...validation.baseValidation,
        value,
        validation: {
          errors,
          dirty,
        },
      },
    };
  };

  let toString = (value: option(string)) => {
    switch (value) {
    | Some(v) => v
    | None => ""
    };
  };

  let make = (~name, ~required, ~maxLength, ~minLength=0, ~initialValue=None, ()) => {
    let valid = {
      baseValidation: {
        validation: {
          errors: [],
          dirty: false,
        },
        name,
        required,
        value:
          switch (initialValue) {
          | Some(v) => v
          | None => ""
          },
      },
      clean: initialValue,
      minLength,
      maxLength,
    };
    let valid = validate(valid, valid.baseValidation.value);
    {
      ...valid,
      baseValidation: {
        ...valid.baseValidation,
        validation: {
          ...valid.baseValidation.validation,
          dirty: false,
        },
      },
    };
  };
};

module PostalCodeValidation = {
  type t = {
    baseValidation,
    clean: option(string),
  };

  let usZipCode = [%re "/\d{5}/g"];

  let validate = (validation: t, value: string) => {
    let dirty = true;
    let clean = String.trim(value);
    let (clean, errors) = {
      let length = String.length(clean);
      if (length > 0) {
        let errors =
          if (length != 5) {
            [
              validation.baseValidation.name ++ " postal code must be 5 digits",
            ];
          } else {
            [];
          };
        let errors =
          if (Js.String.match(usZipCode, clean) == None) {
            List.cons(
              validation.baseValidation.name ++ " invalid zip code.",
              errors,
            );
          } else {
            [];
          };
        if (Belt.List.length(errors) > 0) {
          (None, errors);
        } else {
          (Some(clean), []);
        };
      } else {
        switch (validation.baseValidation.required) {
        | Some(v) => (None, [v])
        | None => (None, [])
        };
      };
    };
    {
      clean,
      baseValidation: {
        ...validation.baseValidation,
        value,
        validation: {
          errors,
          dirty,
        },
      },
    };
  };

  let make = (~name, ~initialValue, ~required) => {
    let valid = {
      baseValidation: {
        validation: {
          errors: [],
          dirty: false,
        },
        name,
        required,
        value: "",
      },
      clean: initialValue,
    };
    let valid = validate(valid, valid.baseValidation.value);
    {
      ...valid,
      baseValidation: {
        ...valid.baseValidation,
        validation: {
          ...valid.baseValidation.validation,
          dirty: false,
        },
      },
    };
  }

  let errors(t) = {
    t.baseValidation.validation.errors
  }

  let validation(t) = {
    t.baseValidation.validation
  }

  let value(t) = {
    t.baseValidation.value
  }
};

module EnumerateValueValidation = {
  type t('a) = {
    validation,
    required: option(string),
    value: option('a),
  };

  let make = (~required, ~initialValue: option('a)) => {
    {
      validation: {
        errors: [],
        dirty: false,
      },
      required,
      value: initialValue,
    };
  };

  let errors(t) = {
    t.validation.errors
  }

  let value(t) = {
    t.value
  }

  let validation(t) = {
    t.validation
  }

  let validate = (validation, value: option('a)) => {
    let dirty = true;
    let errors = {
      switch (validation.required) {
      | Some(e) =>
        if (value == None) {
          [e];
        } else {
          [];
        }
      | None => []
      };
    };
    {
      ...validation,
      validation: {
        errors,
        dirty,
      },
      value,
    };
  };

};

module FloatValidation = {
  type t = {
    baseValidation,
    clean: option(float),
    minValue: float,
    maxValue: float,
  };

  let toString = (value: option(float)) => {
    switch (value) {
    | Some(value) => Js.Float.toString(value)
    | None => ""
    };
  };

  let value(t) = {
    t.value
  }

  let validation(t) = {
    t.validation
  }

  let validate = (validation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) =
      if (String.length(clean) > 0) {
        switch (float_of_string_opt(clean)) {
        | Some(clean) =>
          let errors =
            if (clean < validation.minValue) {
              [
                validation.baseValidation.name
                ++ " must be larger than "
                ++ Js.Float.toString(validation.minValue),
              ];
            } else {
              [];
            };
          let errors =
            if (clean > validation.maxValue) {
              List.cons(
                validation.baseValidation.name
                ++ " must be smaller than "
                ++ Js.Float.toString(validation.maxValue),
                errors,
              );
            } else {
              errors;
            };
          if (Belt.List.length(errors) > 0) {
            (errors, None);
          } else {
            ([], Some(clean));
          };
        | None => (
            [validation.baseValidation.name ++ " invalid numeric value."],
            None,
          )
        };
      } else {
        switch (validation.baseValidation.required) {
        | Some(e) => ([e], None)
        | None => ([], None)
        };
      };
    {
      ...validation,
      clean,
      baseValidation: {
        ...validation.baseValidation,
        value,
        validation: {
          dirty,
          errors,
        },
      },
    };
  };

  let make = (~name, ~required, ~initialValue, ~minValue, ~maxValue) => {
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
      clean: initialValue,
      minValue,
      maxValue,
    };
    validate(valid, valid.baseValidation.value);
  };
};
let isValid = (validation: array(validation), ()) => {
  Belt.Array.length(
    Belt.Array.keep(validation, v => Belt.List.length(v.errors) > 0),
  )
  == 0;
};

module PhoneNumberValidation = {
  type t = {
    baseValidation,
    clean: option(string),
  };
  let allDigits = [%re "/\d+/g"];

  let validation(t) = {
    t.baseValidation.validation
  }

  let value(t) = {
    t.baseValidation.value
  }

  let validate = (validation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) =
      if (String.length(clean) > 0) {
        if (String.length(clean) > 10 && String.length(clean) <= 20) {
          // Make sure they are all digits
          if (Js.String.match(allDigits, clean) == None) {
            (
              [validation.baseValidation.name ++ " phone number is invalid."],
              None,
            );
          } else {
            ([], Some(clean));
          };
        } else {
          (
            [
              validation.baseValidation.name ++ " phone number not long enough.",
            ],
            None,
          );
        };
      } else {
        switch (validation.baseValidation.required) {
        | Some(e) => ([e], None)
        | None => ([], None)
        };
      };
    {
      baseValidation: {
        ...validation.baseValidation,
        validation: {
          dirty,
          errors,
        },
        value,
      },
      clean,
    };
  };

  let make = (~name, ~required, ~initialValue) => {
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
      clean: initialValue,
    };
    let valid = validate(valid, valid.baseValidation.value);
    {
      ...valid,
      baseValidation: {
        ...valid.baseValidation,
        validation: {
          ...valid.baseValidation.validation,
          dirty: false,
        },
      },
    };
  };
};

module IntValidation = {
  type t = {
    baseValidation,
    min: int,
    max: int,
    clean: option(int),
  };

  let value(t) = {
    t.baseValidation.value
  }

  let validation(t) = {
    t.baseValidation.validation
  }

  let toString = (value: option(int)) => {
    switch (value) {
    | Some(i) => string_of_int(i)
    | None => ""
    };
  };

  let validate = (validation, value) => {
    let dirty = true;
    let clean = String.trim(value);
    let (errors, clean) =
      if (String.length(clean) > 0) {
        switch (int_of_string_opt(clean)) {
        | Some(value) =>
          let errors =
            []
            |> (
              errors =>
                (
                  if (value < validation.min) {
                    List.cons(
                      validation.baseValidation.name
                      ++ " must be less than "
                      ++ string_of_int(validation.min),
                      errors,
                    );
                  } else {
                    [];
                  }
                )
                |> (
                  errors =>
                    if (value > validation.max) {
                      List.cons(
                        validation.baseValidation.name
                        ++ " must be less than "
                        ++ string_of_int(validation.max),
                        errors,
                      );
                    } else {
                      [];
                    }
                )
            );
          if (List.length(errors) > 0) {
            (errors, Some(value));
          } else {
            ([], Some(value));
          };
        | None => (["Not a number."], None)
        };
      } else {
        switch (validation.baseValidation.required) {
        | Some(e) => ([e], None)
        | None => ([], None)
        };
      };
    {
      ...validation,
      baseValidation: {
        ...validation.baseValidation,
        validation: {
          dirty,
          errors,
        },
        value,
      },
      clean,
    };
  };

  let make = (~name, ~required, ~initialValue, ~min, ~max) => {
    {
      baseValidation: {
        name,
        validation: {
          dirty: false,
          errors: [],
        },
        value:
          switch (initialValue) {
          | Some(e) => string_of_int(e)
          | None => ""
          },
        required,
      },
      clean: initialValue,
      min,
      max,
    };
  };
};

module UrlValidation = {
  type t = StringValidation.t;

  let make = (~name, ~initialValue, ~required, ~minLength, ~maxLength) => {
    let valid = {
      StringValidation.baseValidation: {
        validation: {
          errors: [],
          dirty: false,
        },
        name,
        required,
        value:
          switch (initialValue) {
          | Some(v) => v
          | None => ""
          },
      },
      clean: initialValue,
      maxLength,
      minLength,
    };

    valid;
  };

  let validUrl: string => bool = [%bs.raw
    {|
        function(url) {
            try {
                new URL(url);
                return true;
            } catch {
                return false
            }
        }
    |}
  ];

  let validate = (validation, value) => {
    let valid = StringValidation.validate(validation, value);
    let (errors, clean) =
      switch (valid.clean) {
      | Some(url) =>
        if (validUrl(url)) {
          (valid.baseValidation.validation.errors, None);
        } else {
          (
            List.cons(
              valid.baseValidation.name ++ " is invalid.",
              valid.baseValidation.validation.errors,
            ),
            valid.clean,
          );
        }
      | None => (valid.baseValidation.validation.errors, valid.clean)
      };
    {
      ...valid,
      baseValidation: {
        ...valid.baseValidation,
        validation: {
          ...valid.baseValidation.validation,
          errors,
        },
      },
      clean,
    };
  };
};

module EmailValidation = {
  type t = StringValidation.t;

  let make = (~name, ~initialValue=None, ~required, ~minLength=0, ~maxLength, ()) => {
    let valid = {
      StringValidation.baseValidation: {
        validation: {
          errors: [],
          dirty: false,
        },
        name,
        required,
        value:
          switch (initialValue) {
          | Some(v) => v
          | None => ""
          },
      },
      clean: initialValue,
      maxLength,
      minLength,
    };
    valid;
  };

  let errors(t: t) = {
    t.baseValidation.validation.errors
  }

  let emailRegex = [%re
    "/[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/g"
  ];

  let validate = (validation, value) => {
    let valid = StringValidation.validate(validation, value);
    let (errors, clean) =
      switch (valid.clean) {
      | Some(v) =>
        let v = Js.String.toLocaleLowerCase(v);
        if (Js.String.match(emailRegex, v) == None) {
          (
            valid.baseValidation.validation.errors
            |> List.cons("Invalid email address."),
            None,
          );
        } else {
          (valid.baseValidation.validation.errors, valid.clean);
        };
      | None => (valid.baseValidation.validation.errors, valid.clean)
      };
    {
      ...valid,
      baseValidation: {
        ...valid.baseValidation,
        validation: {
          ...valid.baseValidation.validation,
          errors,
        },
      },
      clean,
    };
  };
};

module TimeValidation = {
  type t = {
    validation,
    clean: option(MomentRe.Moment.t),
    value: string,
    name: string,
  };

  let make = (~name, ~initialValue) => {
    let date =
      switch (initialValue) {
      | Some(date) => date->MomentRe.Moment.toDate->Js.Date.toLocaleTimeString
      | None => ""
      };
    {
      name,
      clean: initialValue,
      value: date,
      validation: {
        dirty: false,
        errors: [],
      },
    };
  };

  let validate = (validation, value) => {
    let clean = HoytsysCore.Core.DataExt.parseTimeString(value);
    let errors =
      switch (clean) {
      | Some(_) => []
      | None => ["Invalid time: " ++ value]
      };
    {
      ...validation,
      validation: {
        dirty: true,
        errors,
      },
      value,
      clean,
    };
  };
};

module DateValidation = {
  type t = {
    validation,
    value: option(MomentRe.Moment.t),
    name: string,
    required: option(string),
    minDate: MomentRe.Moment.t,
    maxDate: MomentRe.Moment.t,
  };

  let make = (~name, ~required, ~initialValue, ~minDate, ~maxDate) => {
    {
      validation: {
        dirty: false,
        errors: [],
      },
      required,
      value: initialValue,
      name,
      minDate,
      maxDate,
    };
  };

  let validate = (validation, value) => {
    let dirty = true;
    let (value, errors) = {
      switch (value) {
      | Some(d) =>
        let errors =
          if (MomentRe.Moment.isBefore(d, validation.minDate)) {
            [
              "Date must be after "
              ++ DateHelpers.toStringUs(validation.minDate),
            ];
          } else {
            {
              [];
            }
            @ (
              if (MomentRe.Moment.isAfter(d, validation.maxDate)) {
                [
                  "Date must be before "
                  ++ DateHelpers.toStringUs(validation.maxDate),
                ];
              } else {
                [];
              }
            );
          };
        if (List.length(errors) > 0) {
          (None, errors);
        } else {
          (Some(d), errors);
        };
      | None =>
        switch (validation.required) {
        | Some(s) => (None, [s])
        | None => (None, [])
        }
      };
    };
    {
      ...validation,
      validation: {
        dirty,
        errors,
      },
      value,
    };
  };
};
