module TabPanel = {

  type t = {
    tabId: string,
    header: React.element,
    tab: React.element
  }

  let make(header, tab, ~tabId="") = {
    {
      tabId,
      header,
      tab
    }
  }
}

[@react.component]
let make = (~tabs, ~onChange=None, ~tabId=None) => {
  switch(tabs) {
    | [] => <div>
      {React.string("No Tabs")}
    </div>
    | tabs => {
      let (selected, setSelected) = React.useState(_ => 0);
      React.useEffect1(_ => {
        switch (tabId) {
          | Some(tabId) => {
            List.iteri((idx, tab) => {
              if (tab.TabPanel.tabId == tabId) {
                setSelected(_ => idx);
              } else {
                ()
              }
            },tabs);
          }
          | None => ()
        }
        None
      }, [|tabId|]);
      let onSelected(index) {
        setSelected(_ => index);
        switch (onChange, List.nth_opt(tabs, index)) {
          | (Some(onChange), Some(tab)) => {
            onChange(tab.TabPanel.tabId)
          }
          | _ => ()
        }
      }
      // Build an array of the tabs.
      let renderHeader(index, tab) = {
        if (index == selected) {
          <li className="active"
              key={string_of_int(index)}>
            {
              tab.TabPanel.header
            }
          </li>
        } else {
          <li key={string_of_int(index)} onClick={_ => onSelected(index)}>
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
