type columnItem('a) = {
    key: string,
    header: React.element,
    render: ('a) => React.element,
};

type dataset = {
    pages: int,
    total: int
};

type offset = {
    start: int,
    total: int,
};

[@react.component]
let make = (~columns, ~data, ~pageSize, ~rowKey) => {
    let (dataset, setDataset) = React.useState(_ => {
        pages: 0,
        total: 0,
    });
    let (offset, setOffset) = React.useState(_ => {
        start: 0,
        total: 0
    });
    let (currentPage, setCurrentPage) = React.useState(_ => 0);
    React.useEffect2(_ => {
        let total = Belt.Array.length(data);
        let pages = total / pageSize + if (total mod pageSize == 0) {
            0
        } else {
            1
        };
        setDataset(_ => {
            pages,
            total
        });
        setCurrentPage(_ => 0)
        None;
    }, (data, pageSize));
    React.useEffect1(_ => {
        let start = currentPage * pageSize;
        let total = min(start + pageSize, dataset.total);
        setOffset(_ => {
            start,
            total
        });
        None;
    }, [|currentPage|]);
    let selectPage(page) = {
        setCurrentPage(_ => page - 1);
    };
    let prevPage() = {
        setCurrentPage(p => {
            if (p > 0) {
                p - 1;
            } else {
                0
            }
        });
    };
    let nextPage() = {
        setCurrentPage(p => {
            if (p + 1 < dataset.pages) {
                p + 1;
            } else {
                p
            }
        })
    };
    <div className="data-grid">
        <table>
            <thead>
                <tr>
                    {
                        columns
                        -> Belt.Array.map(m => {
                            <th key=m.key>
                                {m.header}
                            </th>
                        })
                        -> React.array
                    }
                </tr>
            </thead>
            <tbody>
                {
                    data
                    -> Belt.Array.slice(~offset=offset.start, ~len=pageSize)
                    -> Belt.Array.map(d => {
                        <tr key={rowKey(d)}>
                            {
                                columns
                                -> Belt.Array.map(c => {
                                    <td key={c.key}>
                                        {c.render(d)}
                                    </td>
                                })
                                -> React.array
                            }
                        </tr>
                    })
                    -> React.array
                }
            </tbody>
        </table>
        <div className="paging">
            <span className="page"
                  onClick={_ => prevPage()}>
                {React.string("<")}
            </span>
            {
                Belt.Array.range(1, dataset.pages)
                -> Belt.Array.map(p => {
                    let className = if (currentPage == p - 1) {
                        "page selected"
                    } else {
                        "page"
                    };
                    <span className=className
                          key={string_of_int(p)}
                          onClick={_ => selectPage(p)}>
                        {React.string(string_of_int(p))}
                    </span>
                })
                -> React.array
            }
            <span className="page"
                  onClick={_ => nextPage()}>
                {React.string(">")}
            </span>
        </div>
    </div>
}