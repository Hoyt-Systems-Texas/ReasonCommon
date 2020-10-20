[@react.component]
let make = (~errors) => {
  switch (errors) {
  | [] => <> </>
  | errors =>
    <ul className="errors">
      {errors
        ->Belt.List.map(error => {
            <li key=error> {React.string(error)} </li>
          })
        ->Array.of_list
        ->React.array}
    </ul>
  };
}