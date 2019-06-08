type values = SynthValues.values;
type index = int;

let getUpdate = (offset, cellSize, values, pageX, pageY) => {
  let (offsetX, offsetY) = offset;

  let x = pageX + offsetX;
  let y = pageY + offsetY;

  let index = Utils.limit(x / cellSize, 0, SynthValues.valuesLength(values) - 1);
  let value = 1. -. (float_of_int(y) /. float_of_int(cellSize));

  let value = Utils.limit(value, 0., 1.);

  SynthValues.{
    index,
    value
  };
};

type viewMode =
  | Inactive
  | Deactive
  | Preview(values, index)
  | Active;

[@react.component]
let make = (~viewMode, ~mapValues, ~getValuesAt, ~values, ~highlightedIndex, ~disabledAfterIndex, ~onAction, ~onSetLength) => {
  let containerRef = React.useRef(Js.Nullable.null);
  let offsetRef = React.useRef((0, 0));

  let (containerOpacity, valueOpacity) = switch (viewMode) {
    | Inactive => ("1", "0")
    | Deactive => ("0.5", "0")
    | Preview(_) => ("1", "1")
    | Active => ("1", "1")
  };

  let cellSize = 48;

  let getUpdateFromMouse = (event) => getUpdate(React.Ref.current(offsetRef), cellSize, values, ReactEvent.Mouse.pageX(event), ReactEvent.Mouse.pageY(event));

  let onMouseEnter = (event) => switch (viewMode) {
    | Inactive | Active => {
      // update offset first.
      let offsetOption = containerRef |> React.Ref.current |> Js.Nullable.toOption;

      React.Ref.setCurrent(offsetRef, Utils.getOffset(offsetOption, 0, 0));

      onAction(getUpdateFromMouse(event), Actions.MouseEnter);
    }
    | Deactive | Preview(_) => ()
  };

  let onMouseDown = (event) => switch (viewMode) {
    | Preview(_) => {
      if (ReactEvent.Mouse.button(event) === 0) {
        let update = getUpdateFromMouse(event);

        if (ReactEvent.Mouse.shiftKey(event)) {
          onSetLength(update.index);
        } else {
          onAction(getUpdateFromMouse(event), MouseDown);
        }
      }
    }
    | Inactive | Deactive | Active => ()
  };

  let onMouseLeave = (event) => switch (viewMode) {
    | Preview(_) | Active => {
      onAction(getUpdateFromMouse(event), MouseLeave);
    }
    | Inactive | Deactive => ()
  };

  React.useEffect4(() => {
    switch (viewMode) {
      | Preview(_) | Active => {
        let getPageX = Webapi.Dom.MouseEvent.pageX;
        let getPageY = Webapi.Dom.MouseEvent.pageY;

        let onMouseMove = (event) => {
          onAction(getUpdate(React.Ref.current(offsetRef), cellSize, values, getPageX(event), getPageY(event)), MouseMove);
        };

        let onMouseUp = (event) => {
          onAction(getUpdate(React.Ref.current(offsetRef), cellSize, values, getPageX(event), getPageY(event)), MouseUp);
        };

        Webapi.Dom.Document.addMouseMoveEventListener(onMouseMove, Webapi.Dom.document);
        Webapi.Dom.Document.addMouseUpEventListener(onMouseUp, Webapi.Dom.document);

        Some(() => {
          Webapi.Dom.Document.removeMouseMoveEventListener(onMouseMove, Webapi.Dom.document);
          Webapi.Dom.Document.removeMouseUpEventListener(onMouseUp, Webapi.Dom.document);
        });
      }
      | _ => None
    };
  }, (values, viewMode, cellSize, onAction));

  <div
    ref={ReactDOMRe.Ref.domRef(containerRef)}
    className="opacity-transition-1 bg-white relative flex-none no-select"
    style=(ReactDOMRe.Style.make(
      ~width=string_of_int(cellSize * SynthValues.valuesLength(values)) ++ "px",
      ~height=string_of_int(cellSize) ++ "px",
      ~opacity=containerOpacity,
      ()
    ))
    onMouseEnter
    onMouseDown
    onMouseLeave
  >
    (mapValues((valueIndex, value:SynthValues.value) => {
      let (scale, previewScale) = switch (viewMode) {
        | Preview(values, index) when index === valueIndex => (getValuesAt(index, values), value.number)
        | Inactive | Deactive | Preview(_) | Active => (value.number, 0.0)
      };

      let isDisabled = valueIndex > disabledAfterIndex;

      <div
        key=string_of_int(valueIndex)
        className=("absolute " ++ (valueIndex === highlightedIndex ? "bg-light-yellow" : isDisabled ? "bg-near-white" : "bg-light-gray"))
        style=(ReactDOMRe.Style.make(
          ~width=string_of_int(cellSize) ++ "px",
          ~height=string_of_int(cellSize) ++ "px",
          ~left=string_of_int(cellSize * valueIndex) ++ "px",
          ()
        ))
      >
        <div
          className=("absolute absolute--fill " ++ (isDisabled ? "bg-moon-gray" : "bg-dark-gray"))
          style=(ReactDOMRe.Style.make(
            ~transformOrigin="0 100%",
            ~transform=("scale3d(1, " ++ Js.Float.toString(scale) ++ ", 1)"),
            ()
          ))
        />
        <div
          className=("absolute absolute--fill bg-hot-pink o-50")
          style=(ReactDOMRe.Style.make(
            ~transformOrigin="0 100%",
            ~transform=("scale3d(1, " ++ Js.Float.toString(previewScale) ++ ", 1)"),
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
          (React.string(value.label))
        </div>
      </div>
    }, values)
    |> React.array)
  </div>
};
