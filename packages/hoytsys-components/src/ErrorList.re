open HoytsysForms.Validation;
[@react.component]
let make = (~errors: validation) => {
    if (errors.dirty) {
        switch (errors.errors) {
            | [] => <>
            </>
            | errors => {
                <ul className="errors">
                    {errors
                        -> Belt.List.map(error => {
                            <li key={error}>
                                {React.string(error)}
                            </li>
                            })
                        -> Array.of_list
                        -> React.array
                    }
                </ul>
            }
        }
    } else {
        <>
        </>
    }
}