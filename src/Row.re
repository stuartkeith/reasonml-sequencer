let component = ReasonReact.statelessComponent("Row");

let make = (~label, ~onRandomiseAbsolute, ~onRandomiseRelative, ~onResetLane, children) => {
  ...component,
  render: _self => {
    <div className="flex items-center">
      <p className="w4">{ReasonReact.string(label)}</p>
      <div className="w1" />
      {children}
      <div className="w1" />
      <div className="flex">
        <button onClick=(_event => onRandomiseAbsolute())>(ReasonReact.string("Random Absolute"))</button>
        <button onClick=(_event => onRandomiseRelative())>(ReasonReact.string("Random Relative"))</button>
        <button onClick=(_event => onResetLane())>(ReasonReact.string("Reset"))</button>
      </div>
    </div>
  }
};
