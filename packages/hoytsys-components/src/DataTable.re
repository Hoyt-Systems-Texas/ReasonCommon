[%bs.raw {|require("./DataTable.scss")|}];

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
    let nextPage() = {
      let nextPage = currentPage + 1;
      if (nextPage < numberOfPages) {
        setPage(nextPage);
      }
    };
    let previousPage() = {
      let prevPage = currentPage - 1;
      if (prevPage >= 0) {
        setPage(prevPage);
      }
    };
    <div className="paging">
      <span className="page"
            onClick={_ => previousPage()}>
        <Octicons.ChevronLeft />
      </span>
      {
        Belt.Array.range(1, numberOfPages)
        -> Belt.Array.map(p => {
          <span className="page"
                onClick={_ => setPage(p - 1)}
                key={string_of_int(p)}>
            {React.string(string_of_int(p))}
          </span>
        })
        -> React.array
      }
      <span className="page"
            onClick={_ => nextPage()}>
        <Octicons.ChevronRight />
      </span>
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
      let start = perPage * page;
      setDataModel(f => {
        switch(f) {
          | Some(f) => Some({
            ...f,
            currentPage: page,
            data: Js.Array.slice(f.allData, ~start, ~end_=start + perPage),
          })
          | None => None
        }
      });
    };
    <div className="data-table">
      <table>
        <thead>
          <tr>
            {
              columns
              -> Belt.Array.map(column => {
                <th key={column.Column.id}>
                  {column.header}
                </th>
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
              <td>
                {React.string("Loading...")}
              </td>
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
