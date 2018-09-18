let module Slider (Config: { type value }) = {
  type index = int;
  type value = Config.value;
  type mouseOver = bool;

  type mouseState =
    | Idle
    | Preview(index, value, value)
    | Active(index, value, mouseOver);

  type state = {
    cellSize: int,
    mouseState,
    rootRef: ref(option(Dom.element)),
    onMouseMoveDom: ref((Webapi.Dom.MouseEvent.t) => unit),
    onMouseUpDom: ref((Webapi.Dom.MouseEvent.t) => unit),
    onMouseMoveReact: ref((ReactEvent.Mouse.t) => unit)
  };

  let limit = (value, min, max) => Pervasives.min(max, Pervasives.max(min, value));

  let getIndexAndValue = (state, cells, pageX, pageY, fromFloat) => {
    let (x, y) = Utils.getOffset(state.rootRef^, pageX, pageY);

    let index = limit(x / state.cellSize, 0, Array.length(cells) - 1);
    let deadZonePixels = 3;

    let ratio = if (y <= deadZonePixels) {
      1.;
    } else if (y >= state.cellSize - deadZonePixels) {
      0.;
    } else {
      let ratio = 1. -. (float_of_int(y) /. float_of_int(state.cellSize));

      limit(ratio, 0., 1.);
    };

    let value = fromFloat(ratio);

    (index, value);
  };

  let getScale = (value, min, max) => float_of_int(value - min) /. float_of_int(max - min);

  let setRootRef = (ref, { ReasonReact.state }) => {
    state.rootRef := Js.Nullable.toOption(ref);
  };

  let noOpEventHandler = (_) => ();

  type action =
    | MouseEnter((index, value))
    | MouseDown((index, value))
    | MouseMove((index, value))
    | MouseUp((index, value))
    | MouseLeave;

  let component = ReasonReact.reducerComponent("Slider");

  let useGlobalMouseEvents = (mouseState) => switch (mouseState) {
    | Idle => false
    | Preview(_) => false
    | Active(_) => true
  };

  let make = (~cells, ~highlightedIndex, ~disabledAfterIndex, ~toFloat:(value => float), ~fromFloat:(float => value), ~getLabel:(value => string), ~onSetValue, ~onSetLength, _children) => {
    let onMouseMove = (getPageX, getPageY, self, event) => {
      self.ReasonReact.send(MouseMove(getIndexAndValue(self.state, cells, getPageX(event), getPageY(event), fromFloat)));
    };

    let onMouseUp = (getPageX, getPageY, self, event) => {
      self.ReasonReact.send(MouseUp(getIndexAndValue(self.state, cells, getPageX(event), getPageY(event), fromFloat)));
    };

    {
      ...component,

      initialState: () => {
        cellSize: 48,
        mouseState: Idle,
        rootRef: ref(None),
        onMouseMoveDom: ref(noOpEventHandler),
        onMouseUpDom: ref(noOpEventHandler),
        onMouseMoveReact: ref(noOpEventHandler)
      },

      reducer: (action, state) => {
        let noOp = state.mouseState;

        let mouseState = switch (state.mouseState, action) {
          | (Idle, MouseEnter((index, value))) => Preview(index, value, cells[index])
          | (Idle, MouseDown(_)) => noOp
          | (Idle, MouseMove((index, value))) => Preview(index, value, cells[index])
          | (Idle, MouseUp(_)) => noOp
          | (Idle, MouseLeave) => noOp
          | (Preview(_), MouseEnter(_)) => noOp
          | (Preview(_), MouseDown((index, value))) => Active(index, value, true)
          | (Preview(pIndex, _pValue, _backupValue), MouseMove((index, value))) when pIndex != index => Preview(index, value, cells[index])
          | (Preview(_pIndex, pValue, backupValue), MouseMove((index, value))) when pValue != value => Preview(index, value, backupValue)
          | (Preview(_), MouseMove(_)) => noOp
          | (Preview(_), MouseUp(_)) => noOp
          | (Preview(_), MouseLeave) => Idle
          | (Active(index, value, _), MouseEnter(_)) => Active(index, value, true)
          | (Active(_), MouseDown(_)) => noOp
          | (Active(_, _, mouseOver), MouseMove((index, value))) => Active(index, value, mouseOver)
          | (Active(_, _, mouseOver), MouseUp((index, value))) when mouseOver => Preview(index, value, value)
          | (Active(_, _, mouseOver), MouseUp(_)) when !mouseOver => Idle
          | (Active(_), MouseUp(_)) => noOp
          | (Active(index, value, _), MouseLeave) => Active(index, value, false)
        };

        let valueToUndo = switch (state.mouseState, mouseState) {
          | (Idle, Idle) => None
          | (Idle, Preview(_)) => None
          | (Idle, Active(_)) =>  None
          | (Preview(index, _, backupValue), Idle) => Some((index, backupValue, false))
          | (Preview(pIndex, _, backupValue), Preview(index, _, _)) when pIndex != index => Some((pIndex, backupValue, false))
          | (Preview(_), Preview(_)) => None
          | (Preview(_), Active(_)) => None
          | (Active(_), Idle) => None
          | (Active(_), Preview(_)) => None
          | (Active(_), Active(_)) => None
        };

        let valueToDo = switch (state.mouseState, mouseState) {
          | (Idle, Idle) => None
          | (Idle, Preview(index, value, _)) => Some((index, value, false))
          | (Idle, Active(index, value, _)) =>  Some((index, value, true))
          | (Preview(_), Idle) => None
          | (Preview(_), Preview(index, value, _backupValue)) => Some((index, value, false))
          | (Preview(_), Active(index, value, _)) => Some((index, value, true))
          | (Active(_), Idle) => None
          | (Active(_), Preview(_)) => None
          | (Active(_), Active(index, value, _)) => Some((index, value, true))
        };

        if (mouseState == state.mouseState) {
          ReasonReact.NoUpdate;
        } else {
          ReasonReact.UpdateWithSideEffects({
            ...state,
            mouseState
          }, (_state) => {
            switch (valueToUndo) {
              | Some((index, value, setLength)) => onSetValue(index, value, setLength)
              | None => ()
            };

            switch (valueToDo) {
              | Some((index, value, setLength)) => onSetValue(index, value, setLength)
              | None => ()
            };
          });
        }
      },

      didMount: (self) => {
        self.state.onMouseMoveDom := onMouseMove(Webapi.Dom.MouseEvent.pageX, Webapi.Dom.MouseEvent.pageY, self);
        self.state.onMouseUpDom := onMouseUp(Webapi.Dom.MouseEvent.pageX, Webapi.Dom.MouseEvent.pageY, self);
        self.state.onMouseMoveReact := onMouseMove(ReactEvent.Mouse.pageX, ReactEvent.Mouse.pageY, self);

        self.onUnmount(() => {
          Webapi.Dom.Document.removeMouseMoveEventListener(self.state.onMouseMoveDom^, Webapi.Dom.document);
          Webapi.Dom.Document.removeMouseUpEventListener(self.state.onMouseUpDom^, Webapi.Dom.document);
        });
      },

      didUpdate: ({ oldSelf, newSelf }) => {
        switch (useGlobalMouseEvents(oldSelf.state.mouseState), useGlobalMouseEvents(newSelf.state.mouseState)) {
          | (false, true) => {
            Webapi.Dom.Document.addMouseMoveEventListener(newSelf.state.onMouseMoveDom^, Webapi.Dom.document);
            Webapi.Dom.Document.addMouseUpEventListener(newSelf.state.onMouseUpDom^, Webapi.Dom.document);
          }
          | (true, false) => {
            Webapi.Dom.Document.removeMouseMoveEventListener(newSelf.state.onMouseMoveDom^, Webapi.Dom.document);
            Webapi.Dom.Document.removeMouseUpEventListener(newSelf.state.onMouseUpDom^, Webapi.Dom.document);
          }
          | (_, _) => ()
        };
      },

      render: self => {
        let { cellSize } = self.state;

        let valueOpacity = switch (self.state.mouseState) {
          | Idle => "0"
          | Preview(_) => "1"
          | Active(_) => "1"
        };

        let onMouseMove = useGlobalMouseEvents(self.state.mouseState) ? noOpEventHandler : self.state.onMouseMoveReact^;

        <div
          ref={self.handle(setRootRef)}
          className="bg-white relative flex-none no-select"
          style=(ReactDOMRe.Style.make(
            ~width=string_of_int(cellSize * Array.length(cells)) ++ "px",
            ~height=string_of_int(cellSize) ++ "px",
            ()
          ))
          onMouseEnter=(event => self.send(MouseEnter(getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat))))
          onMouseDown=(event => {
            if (ReactEvent.Mouse.shiftKey(event)) {
              /* add state instead for mouseMove etc? */
              let (x, _) = getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat);

              onSetLength(x);
            } else {
              self.send(MouseDown(getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat)));
            }
          })
          onMouseLeave=(_event => self.send(MouseLeave))
          onMouseMove
        >
        (ReasonReact.array(Array.mapi((i, value) => {
          let (scale, previewScale) = switch (self.state.mouseState) {
            | Preview(index, _, backupValue) when index == i => (toFloat(backupValue), toFloat(value))
            | Idle | Preview(_) | Active(_) => (toFloat(value), 0.)
          };

          let isDisabled = i > disabledAfterIndex;

          <div
            key=string_of_int(i)
            className=("absolute " ++ (i == highlightedIndex ? "bg-light-yellow" : isDisabled ? "bg-near-white" : "bg-light-gray"))
            style=(ReactDOMRe.Style.make(
              ~width=string_of_int(cellSize) ++ "px",
              ~height=string_of_int(cellSize) ++ "px",
              ~left=string_of_int(cellSize * i) ++ "px",
              ()
            ))
          >
            <div
              className=("absolute absolute--fill " ++ (isDisabled ? "bg-moon-gray" : "bg-dark-gray"))
              style=(ReactDOMRe.Style.make(
                ~transformOrigin="0 100%",
                ~transform=Printf.sprintf("scale3d(1, %g, 1)", scale),
                ()
              ))
            />
            <div
              className=("absolute absolute--fill bg-hot-pink o-50")
              style=(ReactDOMRe.Style.make(
                ~transformOrigin="0 100%",
                ~transform=Printf.sprintf("scale3d(1, %g, 1)", previewScale),
                ()
              ))
            />
            <div
              className="gray absolute absolute--center opacity-transition-1"
              style=(ReactDOMRe.Style.make(
                ~opacity=valueOpacity,
                ()
              ))
            >
              (ReasonReact.string(getLabel(value)))
            </div>
          </div>
        }, cells)))
        </div>
      }
    }
  };
};

module SliderInt = Slider({ type value = int });
module SliderFloat = Slider({ type value = float });
