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
    timeString: A19Forms.Validation.timeValidation,
    timeType: timeType,
    pos: option(int),
};

[@react.component]
let make = (~onChange: (option(Js.Date.t)) => unit, ~name: string) => {
    let (state, setState) = React.useState(_ => {
        timeString: A19Forms.Validation.makeTimeValidation(~name=name, ~initialValue=None),
        timeType: Undetermined,
        pos: None,
    });
    let onKeyPress = (e) => {
        open Webapi.Dom;
        let target: HtmlInputElement.t = ReactEvent.Synthetic.target(e)##valueOf();
        let key = ReactEvent.Synthetic.nativeEvent(e)##code;
        let pos = HtmlInputElement.selectionStart(target);
        switch (key) {
            | "Digit1" | "Digit2" | "Digit3" | "Digit4" | "Digit5" 
            | "Digit6" | "Digit7" | "Digit8" | "Digit9" | "Digit0"
            | "KP_1" | "KP_2" | "KP_3" | "KP_4" | "KP_5" 
            | "KP_6" | "KP_7" | "KP_8" | "KP_9" | "KP_0"
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
                            | _ => ReactEvent.Synthetic.preventDefault(e)
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
                        timeType: TwoDigitHour, pos: Some(pos) })
                }
                | _ => ReactEvent.Synthetic.preventDefault(e)
            }
            | "Space" => switch(pos, state.timeType) {
                | (4, OneDigitHour) 
                | (5, TwoDigitHour) => setState(state => {
                    ...state,
                    pos: Some(pos)
                })
                | _ => ReactEvent.Synthetic.preventDefault(e)
            }
            | "KeyA" | "KeyP" => switch(pos, state.timeType) { | (5, OneDigitHour)
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
        open Webapi.Dom;
        let key = ReactEvent.Synthetic.nativeEvent(e)##code;
        let target: HtmlInputElement.t = ReactEvent.Synthetic.target(e)##valueOf();
        let pos = HtmlInputElement.selectionStart(target);
        Js.Console.log2(pos, key);
        switch (key) {
            | "Backspace" | "Delete" => {
                switch (pos, state.timeType) {
                    | (3, TwoDigitHour)
                    | (2, OneDigitHour) => setState(state => {
                        ...state,
                        pos: None,
                        timeType: Undetermined
                    })
                    | _ => setState(state => {
                        ...state,
                        pos: None,
                    })
                }
            };
            | _ => ()
        }
    };
    let onInputChange = (value) => {
        Js.Console.log(state.pos);
        Js.Console.log(String.length(value));
        let time = switch (state.pos, String.length(value), state.timeType) {
            | (Some(0), 1, OneDigitHour)
            | (Some(1), 2, TwoDigitHour) =>  
                value ++ ":"
            | (Some(3), 4, OneDigitHour)
            | (Some(4), 5, TwoDigitHour) =>  
                value ++ " "
            | (Some(5), 6, OneDigitHour)
            | (Some(6), 7, TwoDigitHour) => 
                value ++ "M"
            | _ => {
                value
            }
        };
        setState(state => {
            ...state,
            timeString: A19Forms.Validation.validationTime(state.timeString, time),
        })
    };
    React.useEffect1(() => {
        onChange(state.timeString.clean);
        None;
    }, [|state|]);
    <>
        <input type_="text"
               value={state.timeString.value}
               onKeyDown={e => onKeyDown(e)}
               onChange={e => onInputChange(ReactEvent.Synthetic.target(e)##value)}
               onKeyPress={e => onKeyPress(e)} />
    </>
}