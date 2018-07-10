let component = ReasonReact.statelessComponent("Lane");

let make = (~label, ~lane:Lane.t, ~onSetValue, ~onSetLength, ~onRandomiseAbsolute, ~onRandomiseRelative, ~onResetLane, _children) => {
  ...component,
  render: _self => {
    <div className="flex items-center">
      <p className="w4">{ReasonReact.string(label)}</p>
      <div className="w1" />
      <div className="flex">
        (ReasonReact.array(Array.mapi((i, value) =>
          <div key=(string_of_int(i)) className=("w2 relative " ++ (i > Lane.loopAfterIndex(lane) ? "o-50" : ""))>
            <button
              disabled=(value === Lane.max(lane))
              className=("input-reset db w-100 h1 " ++ (Lane.visualIndex(lane) === i ? "bg-red" : "bg-gray"))
              onClick=(_event => onSetValue(i, value + 1))
            />
            <button
              disabled=(value === Lane.min(lane))
              className=("input-reset db w-100 h1 " ++ (Lane.visualIndex(lane) === i ? "bg-red" : "bg-gray"))
              onClick=(_event => onSetValue(i, value - 1))
            />
            <p className="relative tc ma0">(ReasonReact.string(string_of_int(value)))</p>
            <button
              className="input-reset db w-100 h1"
              onClick=(_event => onSetLength(i))
            />
          </div>
        , Lane.values(lane))))
      </div>
      <div className="w1" />
      <div className="flex">
        <button onClick=(_event => onRandomiseAbsolute())>(ReasonReact.string("Random Absolute"))</button>
        <button onClick=(_event => onRandomiseRelative())>(ReasonReact.string("Random Relative"))</button>
        <button onClick=(_event => onResetLane())>(ReasonReact.string("Reset"))</button>
      </div>
    </div>
  }
};
