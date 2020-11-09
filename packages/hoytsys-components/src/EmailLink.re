[@react.component]
let make = (~email) => {
  <a href={"mailto:" ++ email}
     target="_blank">
     {React.string(email)}
  </a>
}