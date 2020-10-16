module Column = {
  type t('a) = {
    /**
     * A unique identifier for the column.
     */
    id: string,
    header: React.element,
    data: ('a => React.element),
    show: bool,
  }
}

module type DataTableInfo = {
  type data;
  type column = Column.t(data);

  /**
   * Takes the data value and returns the key.
   */
  let key: data => string;

};

module CurrentPage {
  type t = {
    /**
     * The current page number.
     */
    page: int,
    start: int,
  }
}

module DataInfo {
  type t('a) = {
    data: Js.Array.t('a),
    allData: Js.Array.t('a),
    numberOfPages: int,
    length: int,
    currentPage: int,
  }
}

module TablePage = {
  [@react.component]
  let make = (~currentPage, ~numberOfPages, ~setPage) => {
    <div className="paging">
    </div>
  }
}

module Make_dataTable(I: DataTableInfo) = {
  [@react.component]
  let make = (~perPage, ~data, ~columns) => {
    let (dataModel, setDataModel) = React.useState(_ => None);
    React.useEffect1(_ => {
      switch (data) {
        | Some(d) => {
          let length = Js.Array.length(d);
          let pages = length / perPage;
          let pages = if ((length mod perPage) > 0) {
            pages + 1;
          } else {
            pages
          };
          let model = {
            DataInfo.data: Js.Array.slice(d, ~start=0, ~end_=perPage),
            allData: d,
            numberOfPages: pages,
            length,
            currentPage: 0,
          };
          setDataModel(_ => Some(model));
        }
        | None => {
          setDataModel(_ => None);
        }
      }
      None;
    }, [|data|]);
    let setPage(page) = {
      ()
    };
    <div className="data-table">
      <table>
        <thead>
          <tr>
            {
              columns
              -> Belt.Array.map(column => {
                <td key={column.Column.id}>
                  {column.header}
                </td>
              })
              -> React.array
            }
          </tr>
        </thead>
        <tbody>
        {
          switch(dataModel) {
            | Some(model) => 
            model.data
            -> Belt.Array.map(row => {
              let theKey = I.key(row);
              <tr key=theKey>
                {
                  columns
                  -> Belt.Array.map(col => {
                    <td key={theKey ++ col.id}>
                      {col.data(row)}
                    </td>
                  })
                  -> React.array
                }
              </tr>
            })
            -> React.array
            | None => <tr>
              {React.string("Loading...")}
            </tr>
          }
        }
        </tbody>
      </table>
      {
        switch (dataModel) {
          | Some(d) => {
            <TablePage currentPage=d.currentPage numberOfPages=d.numberOfPages setPage/>
          }
          | None => <>
          </>
        }
      }
    </div>
  }
}
