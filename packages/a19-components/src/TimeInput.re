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
};

type inputState = {
    timeType: timeType,
    pos: option(int)
};

[@react.component]
let make = (~onChange: (option(MomentRe.Moment.t)) => unit, ~name: string, ~time=(None: option(MomentRe.Moment.t))) => {
    let (state, setState) = React.useState(_ => {
        timeString: A19Forms.Validation.makeTimeValidation(~name=name, ~initialValue=None),
    });
    let (inputState, setInputState) = React.useState(_ => {
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
                        switch (pos, inputState.timeType) {
                            | (0, _) => {
                                setInputState(_ => {
                                    pos: Some(pos),
                                    timeType: Undetermined
                                });
                            }
                            | (1, Undetermined) => setInputState(_ => {
                                pos: Some(pos),
                                timeType: TwoDigitHour,
                            });
                            | (2, OneDigitHour)
                            | (3, OneDigitHour)
                            | (3, TwoDigitHour)
                            | (4, TwoDigitHour) => setInputState(state => {
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
                    setInputState(_ => {
                        timeType: OneDigitHour,
                        pos: Some(pos)
                    })
                }
                | 2 => {
                    setInputState(_ => {
                        timeType: TwoDigitHour,
                        pos: Some(pos) })
                }
                | _ => ReactEvent.Synthetic.preventDefault(e)
            }
            | "Space" => switch(pos, inputState.timeType) {
                | (4, OneDigitHour) 
                | (5, TwoDigitHour) => setInputState(state => {
                    ...state,
                    pos: Some(pos)
                })
                | _ => ReactEvent.Synthetic.preventDefault(e)
            }
            | "KeyA" | "KeyP" => switch(pos, inputState.timeType) { 
                | (5, OneDigitHour)
                | (6, TwoDigitHour) => setInputState(state => {
                        ...state,
                        pos: Some(pos)
                    })
                | _ => {
                   ReactEvent.Synthetic.preventDefault(e); 
                }
            } 
            | "KeyM" => switch(pos, inputState.timeType) {
                | (6, OneDigitHour)
                | (7, TwoDigitHour) => setInputState(state => {
                        ...state,
                        pos: Some(pos)
                    })
                | _ => {
                   ReactEvent.Synthetic.preventDefault(e); 
                }
            } 
            | _ => {
                ReactEvent.Synthetic.preventDefault(e)
                setInputState(state => {
                    ...state,
                    pos: None
                });
            }
        };
        Js.Console.log(ReactEvent.Synthetic.isDefaultPrevented(e));
    };
    let onKeyDown = (e) => {
        open Webapi.Dom;
        let key = ReactEvent.Synthetic.nativeEvent(e)##code;
        let target: HtmlInputElement.t = ReactEvent.Synthetic.target(e)##valueOf();
        let pos = HtmlInputElement.selectionStart(target);
        Js.Console.log2(pos, key);
        switch (key) {
            | "Backspace" | "Delete" => {
                switch (pos, inputState.timeType) {
                    | (3, TwoDigitHour)
                    | (2, OneDigitHour) => setInputState(_ => {
                        pos: None,
                        timeType: Undetermined
                    })
                    | _ => setInputState(state => {
                        ...state,
                        pos: None,
                    })
                }
            };
            | _ => ()
        };
    };
    let onInputChange = (value) => {
        Js.Console.log("Onchange called.");
        let time = switch (inputState.pos, String.length(value), inputState.timeType) {
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
            timeString: A19Forms.Validation.validationTime(state.timeString, time),
        });
    };
    React.useEffect1(() => {
        onChange(state.timeString.clean);
        None;
    }, [|state|]);
    React.useEffect1(() => {
        switch (time) {
            | Some(date) => {
                let t = MomentRe.Moment.format("hh:mm A", date);
                setState(s => {
                    timeString: A19Forms.Validation.validationTime(state.timeString, t)
                })
            }
            | None => ()
        }
        None
    }, [|time|]);
    <>
        <input type_="text"
               placeholder="HH:MM AM|PM"
               value={state.timeString.value}
               onKeyDown={e => onKeyDown(e)}
               onChange={e => onInputChange(ReactEvent.Synthetic.target(e)##value)}
               onKeyPress={e => onKeyPress(e)} />
        <ErrorList errors=state.timeString.validation.errors />
    </>
}