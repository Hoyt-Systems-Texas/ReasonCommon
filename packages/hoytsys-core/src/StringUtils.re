let renderOptionString(s) = {
    switch (s) {
        | Some(s) => s
        | None => ""
    }
};

let renderOptionInt(i) = {
    switch (i) {
        | Some(i) => string_of_int(i)
        | None => ""
    }
}