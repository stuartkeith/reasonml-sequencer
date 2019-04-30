let module Slider (Config: { type value }) = {
  type value = Config.value;
  type cells = Lane.values(value);
  type index = int;

  type update = {
    index,
    value,
    cells
  };

  let applyUpdate = (update, cells) => {
    Lane.setValue(cells, update.index, update.value);
  };

  type mousePosition = Inside | Outside;

  type mouseState =
    | Idle
    | Preview(cells, index)
    | Active(mousePosition, cells);

  type state = {
    mouseState,
    offset: ref((int, int)),
    fromFloat: ref(float => value),
    rootRef: ref(option(Dom.element)),
    onMouseMoveDom: ref((Webapi.Dom.MouseEvent.t) => unit),
    onMouseUpDom: ref((Webapi.Dom.MouseEvent.t) => unit)
  };

  let getUpdate = (state, cellSize, cells, fromFloat, pageX, pageY) => {
    let (offsetX, offsetY) = state.offset^;

    let x = pageX + offsetX;
    let y = pageY + offsetY;

    let index = Utils.limit(x / cellSize, 0, Lane.length(cells) - 1);
    let deadZonePixels = 3;

    let ratio = if (y <= deadZonePixels) {
      1.;
    } else if (y >= cellSize - deadZonePixels) {
      0.;
    } else {
      let ratio = 1. -. (float_of_int(y) /. float_of_int(cellSize));

      Utils.limit(ratio, 0., 1.);
    };

    let value = fromFloat(ratio);

    {
      index,
      value,
      cells
    };
  };

  let setRootRef = (ref, { ReasonReact.state }) => {
    state.rootRef := Js.Nullable.toOption(ref);
  };

  let noOpEventHandler = (_) => ();

  type action =
    | MouseEnter(update)
    | MouseMove(update)
    | MouseLeave(update)
    | MouseDown(update)
    | MouseUp(update);

  let cellSize = 48;

  let component = ReasonReact.reducerComponent("Slider");

  let make = (~cells, ~highlightedIndex, ~disabledAfterIndex, ~toFloat:(value => float), ~fromFloat:(float => value), ~getLabel:(value => string), ~onSetValue, ~onSetLength, _children) => {
    {
      ...component,

      initialState: () => {
        mouseState: Idle,
        offset: ref((0, 0)),
        rootRef: ref(None),
        fromFloat: ref(fromFloat),
        onMouseMoveDom: ref(noOpEventHandler),
        onMouseUpDom: ref(noOpEventHandler)
      },

      reducer: (action, state) => switch (state.mouseState, action) {
        | (Idle, MouseEnter(update)) => ReasonReact.UpdateWithSideEffects({
          ...state,
          mouseState: Preview(update.cells, update.index)
        }, (_) => onSetValue(applyUpdate(update, update.cells), None))
        | (Idle, MouseMove(_)) => ReasonReact.NoUpdate
        | (Idle, MouseLeave(_)) => ReasonReact.NoUpdate
        | (Idle, MouseDown(_)) => ReasonReact.NoUpdate
        | (Idle, MouseUp(_)) => ReasonReact.NoUpdate
        | (Preview(_), MouseEnter(_)) => ReasonReact.NoUpdate
        | (Preview(cells, index), MouseMove(update)) when index !== update.index => ReasonReact.UpdateWithSideEffects({
          ...state,
          mouseState: Preview(cells, update.index)
        }, _ => {
          onSetValue(applyUpdate(update, cells), None);
        })
        | (Preview(cells, _), MouseMove(update)) => ReasonReact.SideEffects(_ => {
          onSetValue(applyUpdate(update, cells), None);
        })
        | (Preview(cells, _), MouseLeave(_)) => ReasonReact.UpdateWithSideEffects({
          ...state,
          mouseState: Idle
        }, (_) => onSetValue(cells, None))
        | (Preview(cells, _), MouseDown(_)) => ReasonReact.Update({
          ...state,
          mouseState: Active(Inside, cells)
        })
        | (Preview(_), MouseUp(_)) => ReasonReact.NoUpdate
        | (Active(_, cells), MouseEnter(_)) => ReasonReact.Update({
          ...state,
          mouseState: Active(Inside, cells)
        })
        | (Active(_), MouseMove(update)) => ReasonReact.SideEffects(_ => {
          onSetValue(applyUpdate(update, cells), None);
        })
        | (Active(_, cells), MouseLeave(_)) => ReasonReact.Update({
          ...state,
          mouseState: Active(Outside, cells)
        })
        | (Active(_), MouseDown(_)) => ReasonReact.NoUpdate
        | (Active(mousePosition, cellsToUndo), MouseUp(update)) => ReasonReact.UpdateWithSideEffects({
          ...state,
          mouseState: switch (mousePosition) {
            | Inside => Preview(cells, update.index)
            | Outside => Idle
          }
        }, (_) => {
          onSetValue(applyUpdate(update, cells), Some(cellsToUndo));
        })
      },

      didMount: (self) => {
        let onMouseMove = (self, event) => {
          let pageX = Webapi.Dom.MouseEvent.pageX(event);
          let pageY = Webapi.Dom.MouseEvent.pageY(event);

          self.ReasonReact.send(MouseMove(getUpdate(self.state, cellSize, cells, self.state.fromFloat^, pageX, pageY)));
        };

        let onMouseUp = (self, event) => {
          let pageX = Webapi.Dom.MouseEvent.pageX(event);
          let pageY = Webapi.Dom.MouseEvent.pageY(event);

          self.ReasonReact.send(MouseUp(getUpdate(self.state, cellSize, cells, self.state.fromFloat^, pageX, pageY)));
        };

        self.state.onMouseMoveDom := onMouseMove(self);
        self.state.onMouseUpDom := onMouseUp(self);

        self.onUnmount(() => {
          Webapi.Dom.Document.removeMouseMoveEventListener(self.state.onMouseMoveDom^, Webapi.Dom.document);
          Webapi.Dom.Document.removeMouseUpEventListener(self.state.onMouseUpDom^, Webapi.Dom.document);
        });
      },

      didUpdate: ({ oldSelf, newSelf }) => {
        newSelf.state.fromFloat := fromFloat;

        let wasUsingListeners = switch (oldSelf.state.mouseState) {
          | Idle => false
          | Preview(_) | Active(_) => true
        };

        let isUsingListeners = switch (newSelf.state.mouseState) {
          | Idle => false
          | Preview(_) | Active(_) => true
        };

        switch (wasUsingListeners, isUsingListeners) {
          | (false, true) => {
            Webapi.Dom.Document.addMouseMoveEventListener(newSelf.state.onMouseMoveDom^, Webapi.Dom.document);
            Webapi.Dom.Document.addMouseUpEventListener(newSelf.state.onMouseUpDom^, Webapi.Dom.document);
          }
          | (true, false) => {
            Webapi.Dom.Document.removeMouseMoveEventListener(newSelf.state.onMouseMoveDom^, Webapi.Dom.document);
            Webapi.Dom.Document.removeMouseUpEventListener(newSelf.state.onMouseUpDom^, Webapi.Dom.document);
          }
          | (false, false) => ()
          | (true, true) => ()
        };
      },

      render: self => {
        let valueOpacity = switch (self.state.mouseState) {
          | Idle => "0"
          | Preview(_) => "1"
          | Active(_) => "1"
        };

        let getUpdateFromMouse = (event) => getUpdate(self.state, cellSize, cells, fromFloat, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event));

        <div
          ref={self.handle(setRootRef)}
          className="bg-white relative flex-none no-select"
          style=(ReactDOMRe.Style.make(
            ~width=string_of_int(cellSize * Lane.length(cells)) ++ "px",
            ~height=string_of_int(cellSize) ++ "px",
            ()
          ))
          onMouseEnter=(event => {
            // update offset first
            self.state.offset := Utils.getOffset(self.state.rootRef^, 0, 0);

            self.send(MouseEnter(getUpdateFromMouse(event)));
          })
          onMouseDown=(event => {
            let update = getUpdateFromMouse(event);

            if (ReactEvent.Mouse.shiftKey(event)) {
              onSetLength(update.index);
            } else {
              self.send(MouseDown(update));
            }
          })
          onMouseLeave=(event => self.send(MouseLeave(getUpdateFromMouse(event))))
        >
        (ReasonReact.array(Lane.mapi((i, value) => {
          let (scale, previewScale) = switch (self.state.mouseState) {
            | Preview(cells, index) when index === i => (toFloat(Lane.get(i, cells)), toFloat(value))
            | Preview(_) | Idle | Active(_) => (toFloat(value), 0.)
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
