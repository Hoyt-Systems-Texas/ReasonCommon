[@react.component]
let make = (~formState) => {
  let module F_s = HoytsysCore.Core.FormState;
  switch (formState) {
    | F_s.Loading => {
      <div className="status">
        {React.string("Loading...")}
      </div>
    }
    | F_s.Edit => <>
    </>
    | F_s.Error(e) => 
    <ul className="alert-errors">
      {
        e
        -> Belt.List.map(e => {
          <li>
            {React.string(e)}
          </li>
        })
        -> Array.of_list
        -> React.array
      }
    </ul>
    | F_s.Saving => <div className="status">
      {React.string("Saving...")}
    </div>
  }
}