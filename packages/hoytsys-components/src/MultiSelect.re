[%bs.raw {|require("./MultiSelect.scss")|}];


module type MultiSelectType = {

  type t;
  type identifier;

  include HoytsysCore.StringSearch.String_search_type with type t := t;

  // The display to the text.
  let displayText: t => string;

  // A string representation of the date.
  let getId: t => identifier;

}

module Make_MultiSelect(M: MultiSelectType) = {

  module StringString = HoytsysCore.StringSearch.Make_string_search(M);

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
  let make = (~options) => {
    let (searchText, setSearchText) = React.useState(_ => "");
    let (selected, setSelected) = React.useState(_ => Belt.Set.String.empty);
    let (stringSearch, setStringSearch) = React.useState(_ => None);
    let (searchResult, setSearchResults) = React.useState(_ => options);
    let (hasFocus, setHasFocus) = React.useState(_ => false);

    React.useEffect1(_ => {
      let search = StringString.make(options);
      setStringSearch(_ => Some(search));
      None;
    }, [||]);

    let onSearchText(text) = {
      setSearchText(_ => text);
      switch (stringSearch) {
        | Some(stringSearch) => {
          setSearchResults(_ => StringString.search(text, stringSearch));
          ()
        }
        | None => ()
      };
    };
    let onSelected() = {

    };
    <div className="multi-select">
      <input type_="text"
             className="search-box"
             onFocus={_ => setHasFocus(_ => true)}
             onChange={e => onSearchText(ReactEvent.Synthetic.target(e)##value)}
             value=searchText />
      <div className="tags">
      </div>
      <div className={
          if (hasFocus) {
            "select-drop show"
          } else {
            "select-drop"
          }
        }>
        {
          searchResult
          -> Belt.Array.map((data) => {
            <div className="option">
              {React.string(M.displayText(data))}
            </div>
            })
          -> React.array
        }
      </div>
    </div>
  }
}

