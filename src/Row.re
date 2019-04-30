let options = Array.init(64, i => (string_of_int(i + 1), i + 1));

let module Row (Config: { type value }) = {
  module Slider = Slider.Slider(Config);

  let component = ReasonReact.statelessComponent("Row");

  let make = (~label, ~lane, ~laneType, ~relativeValue, ~sendLaneAction, _children) => {
    ...component,
    render: _self => {
      <div className="flex items-center">
        <p className="w4 flex-none">{ReasonReact.string(label)}</p>
        <div className="w1 flex-none" />
          <select value=(string_of_int(Lane.subTicks(lane))) onChange=((event) => {
            let value = int_of_string(event->ReactEvent.Form.target##value);

            sendLaneAction(laneType, Actions.SetSubTicks(value));
          })>
            (ReasonReact.array(Array.map(((label, _value)) =>
              <option key=label value=label>(ReasonReact.string(label))</option>
            , options)))
          </select>
        <div className="w1 flex-none" />
        <Slider
          cells=Lane.values(lane)
          toFloat=Lane.getParameter(lane).toFloat
          fromFloat=Lane.getParameter(lane).fromFloat
          getLabel=Lane.getParameter(lane).toString
          highlightedIndex=Lane.visualIndex(lane)
          disabledAfterIndex=Lane.loopAfterIndex(lane)
          onSetValue=((array, undoArray) => sendLaneAction(laneType, SetLaneValue(array, undoArray)))
          onSetLength=((index) => sendLaneAction(laneType, SetLoopAfterIndex(index)))
        />
        <div className="w1 flex-none" />
        <div className="flex flex-none">
          <button onClick=(_event => sendLaneAction(laneType, RandomiseLaneAbsolute))>(ReasonReact.string("Random Absolute"))</button>
          <button onClick=(_event => sendLaneAction(laneType, RandomiseLaneRelative(relativeValue)))>(ReasonReact.string("Random Relative"))</button>
          <button onClick=(_event => sendLaneAction(laneType, ResetLane))>(ReasonReact.string("Reset"))</button>
        </div>
      </div>
    }
  };
};

module RowInt = Row({ type value = int });
module RowFloat = Row({ type value = float });
