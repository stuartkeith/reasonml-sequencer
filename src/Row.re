let component = ReasonReact.statelessComponent("Row");

let make = (~label, ~lane:Lane.t, ~onSetValue, ~onSetLength, ~onRandomiseAbsolute, ~onRandomiseRelative, ~onResetLane, _children) => {
  ...component,
  render: _self => {
    <div className="flex items-center">
      <p className="w4">{ReasonReact.string(label)}</p>
      <div className="w1" />
      <Slider
        cells=Lane.values(lane)
        min=Lane.min(lane)
        max=Lane.max(lane)
        highlightedIndex=Lane.visualIndex(lane)
        disabledAfterIndex=Lane.loopAfterIndex(lane)
        onSetValue
        onSetLength
      />
      <div className="w1" />
      <div className="flex">
        <button onClick=(_event => onRandomiseAbsolute())>(ReasonReact.string("Random Absolute"))</button>
        <button onClick=(_event => onRandomiseRelative())>(ReasonReact.string("Random Relative"))</button>
        <button onClick=(_event => onResetLane())>(ReasonReact.string("Reset"))</button>
      </div>
    </div>
  }
};
