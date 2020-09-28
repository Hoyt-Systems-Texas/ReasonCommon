[@react.component]
let make = (~value=None, ~onChange) => {
    let (date, setDate) = React.useState(_ => "");
    React.useEffect1(_ => {
        switch (value) {
            | Some(d) => {
                setDate(_ => HoytsysCore.Date.DateHelpers.toStringIso(d));
            }
            | None => {
                setDate(_ => "");
            }
        }
        None;
    }, [|value|]);
    let onDateChange(d) = {
        setDate(_ => d);
    };
    React.useEffect1(_ => {
        switch (HoytsysCore.Date.DateHelpers.fromStringIso(date)) {
            | Some(d) => {
                onChange(Some(d));
            }
            | None => onChange(None);
        }
        None;
    }, [|date|]);
    <div className="date-picker">
        <input type_="date"
               onChange={e => onDateChange(ReactEvent.Synthetic.target(e)##value)}
               value={date} />
    </div>
}