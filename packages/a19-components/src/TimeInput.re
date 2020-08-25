type amPm = 
    | AM
    | PM
;

type timeType =
    | Undetermined
    | TwoDigitHour
    | OneDigitHour
    ;

type state = {
    timeString: string,
    timeType: timeType,
    pos: option(int),
};

[@react.component]
let make = () => {
    let (state, setState) = React.useState(_ => {
        timeString: "",
        timeType: Undetermined,
        pos: None,
    });
    let onKeyPress = (e) => {
        open Webapi.Dom;
        let target: HtmlInputElement.t = ReactEvent.Synthetic.target(e)##valueOf();
        let key = ReactEvent.Synthetic.nativeEvent(e)##code;
        let pos = HtmlInputElement.selectionStart(target);
        Js.Console.log(key);
        switch (key) {
            | "Digit1" | "Digit2" | "Digit3" | "Digit4" | "Digit5" 
            | "Digit6" | "Digit7" | "Digit8" | "Digit9" | "Digit0"
             =>  {
                switch (pos) {
                    | 0 | 1 | 2 | 3 | 4 => {
                        switch (pos, state.timeType) {
                            | (0, _) => {
                                setState(state => {
                                    ...state,
                                    pos: Some(pos),
                                    timeType: Undetermined
                                })
                            }
                            | (1, Undetermined) => setState(state => {
                                ...state,
                                pos: Some(pos),
                                timeType: TwoDigitHour,
                            })
                            | (2, OneDigitHour)
                            | (3, OneDigitHour)
                            | (3, TwoDigitHour)
                            | (4, TwoDigitHour) => setState(state => {
                                    ...state,
                                    pos: Some(pos)
                            })
                            | _ => ()
                        }
                    }
                    | _ => ReactEvent.Synthetic.preventDefault(e)
                }
            }
            | "Semicolon" => switch (pos) {
                | 1 => {
                    setState(state => {
                        ...state,
                        timeType: OneDigitHour,
                        pos: Some(pos)
                    })
                }
                | 2 => {
                    setState(state => {
                        ...state,
                        timeType: TwoDigitHour,
                        pos: Some(pos)
                    })
                }
                | _ => ReactEvent.Synthetic.preventDefault(e)
            }
            | "Space" => switch(pos, state.timeType) {
                | (4, OneDigitHour) 
                | (5, TwoDigitHour) => setState(state => {
                    ...state,
                    pos: Some(pos)
                })
                | _ => ()
            }
            | "KeyA" | "KeyP" => switch(pos, state.timeType) {
                | (5, OneDigitHour)
                | (6, TwoDigitHour) => setState(state => {
                        ...state,
                        pos: Some(pos)
                    })
                | _ => {
                   ReactEvent.Synthetic.preventDefault(e); 
                }
            } 
            | "KeyM" => switch(pos, state.timeType) {
                | (6, OneDigitHour)
                | (7, TwoDigitHour) => setState(state => {
                        ...state,
                        pos: Some(pos)
                    })
                | _ => {
                   ReactEvent.Synthetic.preventDefault(e); 
                }
            } 
            | _ => {
                setState(state => {
                    ...state,
                    pos: None
                });
                ReactEvent.Synthetic.preventDefault(e)
            }
        }
    };
    let onKeyDown = (e) => {
        let key = ReactEvent.Synthetic.nativeEvent(e)##code;
        Js.Console.log(key);
        switch (key) {
            | "Backspace" | "Delete" => setState(state => {
                ...state,
                pos: None
            });
            | _ => ()
        }
    };
    let onChange = (value) => {
        Js.Console.log(state.pos);
        Js.Console.log(String.length(value));
        switch (state.pos, String.length(value), state.timeType) {
            | (Some(0), 1, OneDigitHour)
            | (Some(1), 2, TwoDigitHour) => setState(state => {
                ...state,
                timeString: value ++ ":"
            })
            | (Some(3), 4, OneDigitHour)
            | (Some(4), 5, TwoDigitHour) => setState(state => {
                ...state,
                timeString: value ++ " "
            })
            | (Some(5), 6, OneDigitHour)
            | (Some(6), 7, TwoDigitHour) => setState(state => {
                ...state,
                timeString: value ++ "M"
            })
            | _ => {

                setState(state => {
                    ...state,
                    timeString: value
                })
            }
        }
    };
    <>
        <input type_="text"
               value={state.timeString}
               onKeyDown={e => onKeyDown(e)}
               onChange={e => onChange(ReactEvent.Synthetic.target(e)##value)}
               onKeyPress={e => onKeyPress(e)} />
    </>
}