[@react.component]
let make = (~children, ~page) => {
    let onClick(e) = {
        ReactEvent.Synthetic.preventDefault(e);
        ReasonReactRouter.push(page);
    };
    <a href={page} onClick={e => onClick(e)}>
        {children}
    </a>
}