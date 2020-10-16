module Checklist = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "ChecklistIcon";
    
}

module ChevronRight = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "ChevronRightIcon";
    
}

module ChevronLeft = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "ChevronLeftIcon";
}

module Person = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "PersonIcon";
};

module Organization = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "OrganizationIcon";
};

module ThreeBars = {

    [@bs.module "@primer/octicons-react"][@react.component]
    external make: (~size:int=?) => React.element = "ThreeBarsIcon";
};