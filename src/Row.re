let component = ReasonReact.statelessComponent("Lane");

let make = (~label, ~lane:Lane.lane, ~onSetValue, ~onSetLength, _children) => {
  ...component,
  render: _self => {
    <div>
      <p>{ReasonReact.string(label)}</p>
      <div className="flex">
        (ReasonReact.array(Array.mapi((i, value) =>
          <div key=(string_of_int(i)) className=("w2 relative " ++ (i > lane.loopAfterIndex ? "o-50" : ""))>
            <button className=("input-reset db w-100 h1 " ++ (lane.visualIndex === i ? "bg-red" : "bg-gray")) onClick=(_event => onSetValue(i, value + 1)) />
            <button className=("input-reset db w-100 h1 " ++ (lane.visualIndex === i ? "bg-red" : "bg-gray")) onClick=(_event => onSetValue(i, value - 1)) />
            <p className="relative tc ma0">(ReasonReact.string(string_of_int(value)))</p>
            <button className="input-reset db w-100 h1" onClick=(_event => onSetLength(i)) />
          </div>
        , lane.values)))
      </div>
    </div>
  }
};
