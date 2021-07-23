type values = SynthValues.values;
type index = int;

let getUpdate = (offset, cellSize, values, pageX, pageY) => {
  let (offsetX, offsetY) = offset;

  let x = pageX + offsetX;
  let y = pageY + offsetY;

  let index = Utils.limit(x / cellSize, 0, SynthValues.length(values) - 1);
  let value = 1. -. (float_of_int(y) /. float_of_int(cellSize));

  let value = Utils.limit(value, 0., 1.);

  let values: SynthValues.update = {
    index,
    value
  };

  values;
};

type viewMode =
  | Inactive
  | Deactive
  | Preview(values, index)
  | Active;

@react.component
let make = (~cellSize, ~viewMode, ~mapValues, ~getValueAt, ~values, ~disabledIndex, ~onAction, ~onSetLength) => {
  let containerRef = React.useRef(Js.Nullable.null);
  let offsetRef = React.useRef((0, 0));

  let (containerOpacity, showLabels) = switch (viewMode) {
    | Inactive => ("1", false)
    | Deactive => ("0.5", false)
    | Preview(_) => ("1", true)
    | Active => ("1", true)
  };

  let getUpdateFromMouse = (event) => getUpdate(offsetRef.current, cellSize, values, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event));

  let onMouseEnter = (event) => {
    // update offset first.
    let offsetOption = containerRef.current |> Js.Nullable.toOption;

    offsetRef.current = Utils.getOffset(offsetOption, 0, 0);

    onAction(getUpdateFromMouse(event), TrackEditMode.MouseEnter);
  };

  let onMouseDown = (event) => switch (viewMode) {
    | Preview(_) => {
      if (ReactEvent.Mouse.button(event) === 0) {
        // Safari will show the select cursor unless prevented.
        ReactEvent.Mouse.preventDefault(event);

        let update = getUpdateFromMouse(event);

        if (ReactEvent.Mouse.shiftKey(event)) {
          onSetLength(update.index + 1);
        } else {
          onAction(getUpdateFromMouse(event), MouseDown);
        }
      }
    }
    | Inactive | Deactive | Active => ()
  };

  let onMouseLeave = (event) => {
    onAction(getUpdateFromMouse(event), MouseLeave);
  };

  let onMouseMove = (event) => switch (viewMode) {
    | Deactive=> onAction(getUpdateFromMouse(event), MouseMove)
    | Inactive | Preview(_) | Active => ()
  };

  React.useEffect4(() => {
    switch (viewMode) {
      | Preview(_) | Active => {
        let getPageX = Webapi.Dom.MouseEvent.pageX;
        let getPageY = Webapi.Dom.MouseEvent.pageY;

        let onMouseMove = (event) => {
          onAction(getUpdate(offsetRef.current, cellSize, values, getPageX(event), getPageY(event)), MouseMove);
        };

        let onMouseUp = (event) => {
          onAction(getUpdate(offsetRef.current, cellSize, values, getPageX(event), getPageY(event)), MouseUp);
        };

        Webapi.Dom.Document.addMouseMoveEventListener(onMouseMove, Webapi.Dom.document);
        Webapi.Dom.Document.addMouseUpEventListener(onMouseUp, Webapi.Dom.document);

        Some(() => {
          Webapi.Dom.Document.removeMouseMoveEventListener(onMouseMove, Webapi.Dom.document);
          Webapi.Dom.Document.removeMouseUpEventListener(onMouseUp, Webapi.Dom.document);
        });
      }
      | Deactive | Inactive => None
    };
  }, (values, viewMode, cellSize, onAction));

  <div
    ref={ReactDOM.Ref.domRef(containerRef)}
    className="opacity-transition-1 bg-white relative flex-none no-select"
    style=(ReactDOM.Style.make(
      ~width=string_of_int(cellSize * SynthValues.length(values)) ++ "px",
      ~height=string_of_int(cellSize) ++ "px",
      ~opacity=containerOpacity,
      ()
    ))
    onMouseEnter
    onMouseDown
    onMouseLeave
    onMouseMove
  >
    (mapValues((valueIndex, value, label) => {
      let (scale, previewScale) = switch (viewMode) {
        | Preview(values, index) when index === valueIndex => (getValueAt(index, values), value)
        | Inactive | Deactive | Preview(_) | Active => (value, 0.0)
      };

      let isDisabled = valueIndex >= disabledIndex;

      <div
        key=string_of_int(valueIndex)
        className=("absolute " ++ (isDisabled ? "bg-near-white" : "bg-light-gray"))
        style=(ReactDOM.Style.make(
          ~width=string_of_int(cellSize) ++ "px",
          ~height=string_of_int(cellSize) ++ "px",
          ~left=string_of_int(cellSize * valueIndex) ++ "px",
          ()
        ))
      >
        <div
          className=("absolute absolute--fill " ++ (isDisabled ? "bg-moon-gray" : "bg-dark-gray"))
          style=(ReactDOM.Style.make(
            ~transformOrigin="0 100%",
            ~transform=("scale3d(1, " ++ Js.Float.toString(scale) ++ ", 1)"),
            ()
          ))
        />
        <div
          className=("absolute absolute--fill bg-hot-pink o-50")
          style=(ReactDOM.Style.make(
            ~transformOrigin="0 100%",
            ~transform=("scale3d(1, " ++ Js.Float.toString(previewScale) ++ ", 1)"),
            ()
          ))
        />
        (showLabels ?
          <div className="gray absolute absolute--center opacity-transition-1">
            (React.string(label))
          </div>
        : React.null)
      </div>
    }, values)
    |> React.array)
  </div>
};

let make = React.memo(make);
