let component = ReasonReact.statelessComponent("Row");

let make = (~label, ~lane:Lane.t, ~onSetValue, ~onSetLength, ~onRandomiseAbsolute, ~onRandomiseRelative, ~onResetLane, _children) => {
  ...component,
  render: _self => {
    <div className="flex items-center">
      <p className="w4">{ReasonReact.string(label)}</p>
      <div className="w1" />
      <Slider.SliderInt
        cells=Lane.values(lane)
        toFloat=(value => float_of_int(value - Lane.min(lane)) /. float_of_int(Lane.max(lane) - Lane.min(lane)))
        fromFloat=(value => Lane.min(lane) + int_of_float(float_of_int(Lane.max(lane) - Lane.min(lane)) *. value))
        getLabel=string_of_int
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
