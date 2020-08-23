type resultMonad('a) = 
    | Success('a)
    | Error(list(string))
    | Busy
    | AccessDenied
    ;