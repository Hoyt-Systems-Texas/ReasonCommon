module AccordionGroup {

    type t = {
        key: string,
        parent: React.element,
        children: Js.Array.t(React.element),
    };
};

module AccordionChildren {

    [@react.component]
    let make = (~children) => {
        <div className="children">
            {children}
        </div>
    }
}

[@react.component]
let make = (~menus: Js.Array.t(AccordionGroup.t)) => {
    let (active, setActive) = React.useState(_ => None);
    let onSelect(key) = {
        setActive(_ => Some(key));
    };
    <div className="accordion">
        {
            menus
            -> Belt.Array.map(g => {
                let className = if (active == Some(g.key)) {
                    "group expand"
                } else {
                    "group"
                };
                <div className=className
                     key={g.key}
                     onClick={_ => onSelect(g.key)}>
                    { g.parent }
                    {
                        if (active == Some(g.key)) {
                            <AccordionChildren>
                                {
                                    g.children
                                    -> React.array
                                }
                            </AccordionChildren>
                        } else {
                            <>
                            </>
                        }
                    }
                </div>
            })
            -> React.array
        }
    </div>
}