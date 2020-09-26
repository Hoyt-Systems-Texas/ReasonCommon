[@react.component]
let make = (~errors: list(string)) => {
    switch (errors) {
        | [] => <>
        </>
        | errors => {
            <ul className="errors">
                {errors
                    -> Belt.List.map(error => {
                        <li>
                            {React.string(error)}
                        </li>
                        })
                    -> Array.of_list
                    -> React.array
                }
            </ul>
        }
    }
}