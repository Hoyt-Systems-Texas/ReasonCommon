[%bs.raw {|require("./MultiSelect.scss")|}];

module type MultiSelectType = {

  type t;

  // The display to the text.
  let displayText: t => string;

  // A string representation of the date.
  let id: t => string;

  // Used to get the children.
  let get_children: t => list(t);
}

module Make_MultiSelect(M: MultiSelectType) = {

  module SelectTag = {

    [@react.component]
    let make = (~tag, ~onDelete) => {
      let (text, setText) = React.useState(_ => M.displayText(tag));
      let (currentTag, setCurrentTag) = React.useState(_ => tag);

      React.useEffect1(_ => {
        setText(_ => M.displayText(tag));
        setCurrentTag(_ => tag);
        None;
       }, [|tag|]);

      <span className="select-pill">
        {React.string(text)}
        <button type_="button"
                onClick={_ => onDelete(currentTag)}>
          {React.string("X")}
        </button>
      </span>
    }
  }

  [@react.component]
  let make = (~values) => {
    let (searchText, setSearchText) = React.useState(_ => "");
    let (selected, setSelected) = React.useState(_ => Belt.Set.String.empty);

    let onSearchText(text) = {
      setSearchText(_ => text);
    };
    let onSelected() = {

    };
    <div className="multi-select">
      <div className="tags">
      </div>
      <input type_="text"
             className="search-box"
             onChange={e => onSearchText(ReactEvent.Synthetic.target(e)##value)}
             value=searchText />
    </div>
  }

}

