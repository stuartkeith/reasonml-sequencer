type value = float;
type values = SynthValues.values;
type index = int;

type update = {
  index,
  value,
  values
};

type mousePosition = Inside | Outside;

type mouseState =
  | Idle
  | Preview(values, index)
  | Active(mousePosition, values);

type state = {
  mouseState,
  offset: ref((int, int)),
  rootRef: ref(option(Dom.element)),
  onMouseMoveDom: ref((Webapi.Dom.MouseEvent.t) => unit),
  onMouseUpDom: ref((Webapi.Dom.MouseEvent.t) => unit)
};

let getUpdate = (state, cellSize, values, pageX, pageY) => {
  let (offsetX, offsetY) = state.offset^;

  let x = pageX + offsetX;
  let y = pageY + offsetY;

  let index = Utils.limit(x / cellSize, 0, SynthValues.valuesLength(values) - 1);
  let deadZonePixels = 3;

  let value = if (y <= deadZonePixels) {
    1.;
  } else if (y >= cellSize - deadZonePixels) {
    0.;
  } else {
    let value = 1. -. (float_of_int(y) /. float_of_int(cellSize));

    Utils.limit(value, 0., 1.);
  };

  {
    index,
    value,
    values
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

let make = (~updateValues, ~mapValues, ~getValuesAt, ~values, ~highlightedIndex, ~disabledAfterIndex, ~onSetValue, ~onSetLength, _children) => {
  {
    ...component,

    initialState: () => {
      mouseState: Idle,
      offset: ref((0, 0)),
      rootRef: ref(None),
      onMouseMoveDom: ref(noOpEventHandler),
      onMouseUpDom: ref(noOpEventHandler)
    },

    reducer: (action, state) => switch (state.mouseState, action) {
      | (Idle, MouseEnter(update)) => ReasonReact.UpdateWithSideEffects({
        ...state,
        mouseState: Preview(update.values, update.index)
      }, (_) => onSetValue(updateValues(update.values, update.index, update.value), None))
      | (Idle, MouseMove(_)) => ReasonReact.NoUpdate
      | (Idle, MouseLeave(_)) => ReasonReact.NoUpdate
      | (Idle, MouseDown(_)) => ReasonReact.NoUpdate
      | (Idle, MouseUp(_)) => ReasonReact.NoUpdate
      | (Preview(_), MouseEnter(_)) => ReasonReact.NoUpdate
      | (Preview(values, index), MouseMove(update)) when index !== update.index => ReasonReact.UpdateWithSideEffects({
        ...state,
        mouseState: Preview(values, update.index)
      }, _ => {
        onSetValue(updateValues(values, update.index, update.value), None);
      })
      | (Preview(values, _), MouseMove(update)) => ReasonReact.SideEffects(_ => {
        onSetValue(updateValues(values, update.index, update.value), None);
      })
      | (Preview(values, _), MouseLeave(_)) => ReasonReact.UpdateWithSideEffects({
        ...state,
        mouseState: Idle
      }, (_) => onSetValue(values, None))
      | (Preview(values, _), MouseDown(_)) => ReasonReact.Update({
        ...state,
        mouseState: Active(Inside, values)
      })
      | (Preview(_), MouseUp(_)) => ReasonReact.NoUpdate
      | (Active(_, values), MouseEnter(_)) => ReasonReact.Update({
        ...state,
        mouseState: Active(Inside, values)
      })
      | (Active(_), MouseMove(update)) => ReasonReact.SideEffects(_ => {
        onSetValue(updateValues(values, update.index, update.value), None);
      })
      | (Active(_, values), MouseLeave(_)) => ReasonReact.Update({
        ...state,
        mouseState: Active(Outside, values)
      })
      | (Active(_), MouseDown(_)) => ReasonReact.NoUpdate
      | (Active(mousePosition, valuesToUndo), MouseUp(update)) => ReasonReact.UpdateWithSideEffects({
        ...state,
        mouseState: switch (mousePosition) {
          | Inside => Preview(values, update.index)
          | Outside => Idle
        }
      }, (_) => {
        onSetValue(updateValues(values, update.index, update.value), Some(valuesToUndo));
      })
    },

    didMount: (self) => {
      let onMouseMove = (self, event) => {
        let pageX = Webapi.Dom.MouseEvent.pageX(event);
        let pageY = Webapi.Dom.MouseEvent.pageY(event);

        self.ReasonReact.send(MouseMove(getUpdate(self.state, cellSize, values, pageX, pageY)));
      };

      let onMouseUp = (self, event) => {
        let pageX = Webapi.Dom.MouseEvent.pageX(event);
        let pageY = Webapi.Dom.MouseEvent.pageY(event);

        self.ReasonReact.send(MouseUp(getUpdate(self.state, cellSize, values, pageX, pageY)));
      };

      self.state.onMouseMoveDom := onMouseMove(self);
      self.state.onMouseUpDom := onMouseUp(self);

      self.onUnmount(() => {
        Webapi.Dom.Document.removeMouseMoveEventListener(self.state.onMouseMoveDom^, Webapi.Dom.document);
        Webapi.Dom.Document.removeMouseUpEventListener(self.state.onMouseUpDom^, Webapi.Dom.document);
      });
    },

    didUpdate: ({ oldSelf, newSelf }) => {
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

      let getUpdateFromMouse = (event) => getUpdate(self.state, cellSize, values, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event));

      <div
        ref=self.handle(setRootRef)
        className="bg-white relative flex-none no-select"
        style=(ReactDOMRe.Style.make(
          ~width=string_of_int(cellSize * SynthValues.valuesLength(values)) ++ "px",
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
      (mapValues((i, value:SynthValues.value) => {
        let (scale, previewScale) = switch (self.state.mouseState) {
          | Preview(values, index) when index === i => (getValuesAt(i, values), value.number)
          | Preview(_) | Idle | Active(_) => (value.number, 0.0)
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
            (ReasonReact.string(value.label))
          </div>
        </div>
      }, values)
      |> ReasonReact.array)
      </div>
    }
  }
};
