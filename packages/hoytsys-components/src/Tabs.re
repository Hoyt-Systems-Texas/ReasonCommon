module TabPanel = {

  type t = {
    header: React.element,
    tab: React.element
  }

  let make(header, tab) = {
    {
      header,
      tab
    }
  }
}

[@react.component]
let make = (~tabs) => {
  switch(tabs) {
    | [] => <div>
      {React.string("No Tabs")}
    </div>
    | tabs => {
      let (selected, setSelected) = React.useState(_ => 0);
      // Build an array of the tabs.
      let renderHeader(index, tab) = {
        if (index == selected) {
          <li className="active">
            {
              tab.TabPanel.header
            }
          </li>
        } else {
          <li onClick={_ => setSelected(_ => index)}>
            {
              tab.TabPanel.header
            }
          </li>
        };
      };
      <div className="tabs">
        <ul role="nav">
          {
            tabs
            -> Belt.List.mapWithIndex(renderHeader)
            -> Array.of_list
            -> React.array
          }
        </ul>
        <div className="body">
          {
            switch (Belt.List.get(tabs, selected)) {
              | Some(tab) => {
                tab.TabPanel.tab
              }
              | None => {
                <>
                  {React.string("Tab out of the range.")}
                </>
              }
            }
          }
        </div>
      </div>
    }
  }
}
