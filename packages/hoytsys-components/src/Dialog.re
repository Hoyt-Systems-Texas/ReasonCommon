[%bs.raw {|require("./Dialog.scss")|}];

[@react.component]
let make = (~show, ~children) => {
  let className = if (show) {
    "dialog show"
  } else {
    "dialog"
  };
  <div className>
    <div className="popup">
      {children}
    </div>
  </div>
}