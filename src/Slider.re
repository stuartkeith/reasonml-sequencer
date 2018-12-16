let module Slider (Config: { type value }) = {
  type index = int;
  type value = Config.value;

  type cellUpdate =
    | NoUpdate
    | UpdateUndo(Lane.values(value), Lane.values(value))
    | UpdateNoUndo(Lane.values(value));

  type mousePosition = Inside | Outside;

  type mouseState =
    | Idle
    | Preview(index, value, Lane.values(value))
    | Active(index, value, mousePosition, Lane.values(value));

  type state = {
    cellSize: int,
    mouseState,
    offset: ref((int, int)),
    fromFloat: ref(float => value),
    rootRef: ref(option(Dom.element)),
    onMouseMoveDom: ref((Webapi.Dom.MouseEvent.t) => unit),
    onMouseUpDom: ref((Webapi.Dom.MouseEvent.t) => unit),
    onMouseMoveReact: ref((ReactEvent.Mouse.t) => unit)
  };

  let getIndexAndValue = (state, cells, pageX, pageY, fromFloat, getOffset) => {
    if (getOffset) {
      state.offset := Utils.getOffset(state.rootRef^, 0, 0);
    };

    let (offsetX, offsetY) = state.offset^;

    let x = pageX + offsetX;
    let y = pageY + offsetY;

    let index = Utils.limit(x / state.cellSize, 0, Lane.length(cells) - 1);
    let deadZonePixels = 3;

    let ratio = if (y <= deadZonePixels) {
      1.;
    } else if (y >= state.cellSize - deadZonePixels) {
      0.;
    } else {
      let ratio = 1. -. (float_of_int(y) /. float_of_int(state.cellSize));

      Utils.limit(ratio, 0., 1.);
    };

    let value = fromFloat(ratio);

    (index, value);
  };

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
      self.ReasonReact.send(MouseMove(getIndexAndValue(self.state, cells, getPageX(event), getPageY(event), self.state.fromFloat^, false)));
    };

    let onMouseUp = (getPageX, getPageY, self, event) => {
      self.ReasonReact.send(MouseUp(getIndexAndValue(self.state, cells, getPageX(event), getPageY(event), self.state.fromFloat^, false)));
    };

    {
      ...component,

      initialState: () => {
        cellSize: 48,
        mouseState: Idle,
        offset: ref((0, 0)),
        rootRef: ref(None),
        fromFloat: ref(fromFloat),
        onMouseMoveDom: ref(noOpEventHandler),
        onMouseUpDom: ref(noOpEventHandler),
        onMouseMoveReact: ref(noOpEventHandler)
      },

      reducer: (action, state) => {
        let noOp = (state.mouseState, NoUpdate);

        let (mouseState, cellsToDispatch) = switch (state.mouseState, action) {
          | (Idle, MouseEnter((index, value))) => (Preview(index, value, cells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Idle, MouseDown(_)) => noOp
          | (Idle, MouseMove((index, value))) => (Preview(index, value, cells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Idle, MouseUp(_)) => noOp
          | (Idle, MouseLeave) => noOp
          | (Preview(_), MouseEnter(_)) => noOp
          | (Preview(_, _, pCells), MouseDown((index, value))) => (Active(index, value, Inside, pCells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Preview(pIndex, _, pCells), MouseMove((index, value))) when pIndex != index => (Preview(index, value, pCells), UpdateNoUndo(pCells))
          | (Preview(_, pValue, pCells), MouseMove((index, value))) when pValue != value => (Preview(index, value, pCells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Preview(_), MouseMove(_)) => noOp
          | (Preview(_), MouseUp(_)) => noOp
          | (Preview(_, _, pCells), MouseLeave) => (Idle, UpdateNoUndo(pCells))
          | (Active(index, value, _, undoCells), MouseEnter(_)) => (Active(index, value, Inside, undoCells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Active(_), MouseDown(_)) => noOp
          | (Active(_, _, _, undoCells), MouseMove((index, value))) => (Active(index, value, Inside, undoCells), UpdateNoUndo(Lane.setValue(cells, index, value)))
          | (Active(_, _, mousePosition, undoCells), MouseUp((index, value))) when mousePosition == Inside => (Preview(index, value, cells), UpdateUndo(cells, undoCells))
          | (Active(_, _, mousePosition, undoCells), MouseUp(_)) when mousePosition == Outside => (Idle, UpdateUndo(cells, undoCells))
          | (Active(_), MouseUp(_)) => noOp
          | (Active(index, value, _, undoCells), MouseLeave) => (Active(index, value, Outside, undoCells), NoUpdate)
        };

        ReasonReact.UpdateWithSideEffects({
          ...state,
          mouseState
        }, (_state) => {
          switch (cellsToDispatch) {
            | UpdateNoUndo(cellsToDispatch) => onSetValue(cellsToDispatch, None)
            | UpdateUndo(cellsToDispatch, blah) => onSetValue(cellsToDispatch, Some(blah))
            | NoUpdate => ()
          };
        });
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
        newSelf.state.fromFloat := fromFloat;

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
            ~width=string_of_int(cellSize * Lane.length(cells)) ++ "px",
            ~height=string_of_int(cellSize) ++ "px",
            ()
          ))
          onMouseEnter=(event => self.send(MouseEnter(getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat, true))))
          onMouseDown=(event => {
            if (ReactEvent.Mouse.shiftKey(event)) {
              /* add state instead for mouseMove etc? */
              let (x, _) = getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat, false);

              onSetLength(x);
            } else {
              self.send(MouseDown(getIndexAndValue(self.state, cells, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event), fromFloat, false)));
            }
          })
          onMouseLeave=(_event => self.send(MouseLeave))
          onMouseMove
        >
        (ReasonReact.array(Lane.mapi((i, value) => {
          let (scale, previewScale) = switch (self.state.mouseState) {
            | Preview(index, _, previewCells) when index == i => (toFloat(Lane.get(i, previewCells)), toFloat(value))
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
