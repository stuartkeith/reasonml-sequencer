[@react.component]
let make = (~cellSize, ~highlightedIndex, ~disabledIndex, ~length, ~onSetLength) => {
  let height = "16px";

  <div
    className="relative flex-none"
    style=(ReactDOMRe.Style.make(
      ~width=string_of_int(cellSize * length) ++ "px",
      ~height,
      ()
    ))
  >
    (Array.mapi((index, _) => {
      let isDisabled = index >= disabledIndex;

      let backgroundClassName = if (isDisabled) {
        "bg-light-gray";
      } else if (index === highlightedIndex) {
        "bg-gold";
      } else {
        "bg-white";
      };

      let onClick = (_event) => {
        onSetLength(index + 1);
      };

      <button
        key=string_of_int(index)
        className=("input-reset absolute " ++ backgroundClassName)
        style=(ReactDOMRe.Style.make(
          ~width=string_of_int(cellSize) ++ "px",
          ~height,
          ~left=string_of_int(cellSize * index) ++ "px",
          ()
        ))
        onClick
      />
    }, Array.make(length, None))
    |> React.array)
  </div>
};

let make = React.memo(make);
