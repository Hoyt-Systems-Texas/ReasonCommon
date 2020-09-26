[@react.component]
let make = (~date=None, ~onChange) => {
    <div className="date-picker">
        <input type_="date" />
    </div>
}