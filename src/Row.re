let module Row (Config: { type value }) = {
  module Slider = Slider.Slider(Config);

  let component = ReasonReact.statelessComponent("Row");

  let make = (~label, ~lane, ~onRandomiseAbsolute, ~onRandomiseRelative, ~onResetLane, ~onSetValue, ~onSetLength, _children) => {
    ...component,
    render: _self => {
      <div className="flex items-center">
        <p className="w4">{ReasonReact.string(label)}</p>
        <div className="w1" />
        <Slider
          cells=Lane.values(lane)
          toFloat=Lane.getParameter(lane).toFloat
          fromFloat=Lane.getParameter(lane).fromFloat
          getLabel=Lane.getParameter(lane).toString
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
};

module RowInt = Row({ type value = int });
module RowFloat = Row({ type value = float });
