let options = Array.init(64, i => (string_of_int(i + 1), i + 1));

let component = ReasonReact.statelessComponent("Row");

let make = (~synthTrack:SynthTrack.t, ~globalParameters:SynthParameters.globalParameters, ~send, _children) => {
  ...component,
  render: _self => {
    <div className="flex items-center">
      <p className="ma0 w4 flex-none">(ReasonReact.string(synthTrack.label))</p>
      <div className="w1 flex-none" />
        <select
          value=(string_of_int(synthTrack.subTicks))
          onChange=((event) => {
            let value = int_of_string(event->ReactEvent.Form.target##value);

            send(Actions.SetSubTicks(synthTrack.id, value));
          })
        >
          (ReasonReact.array(Array.map(((label, _value)) => {
            <option key=label value=label>(ReasonReact.string(label))</option>
          }, options)))
        </select>
      <div className="w1 flex-none" />
      <Slider
        updateValues=SynthValues.updateValues(globalParameters, synthTrack.valueConverter)
        mapValues=SynthValues.mapValues(globalParameters, synthTrack.valueConverter)
        getValuesAt=SynthValues.getValuesAt(globalParameters, synthTrack.valueConverter)
        values=synthTrack.values
        highlightedIndex=Timing.index(synthTrack.loopAfterIndex, synthTrack.timing)
        disabledAfterIndex=synthTrack.loopAfterIndex
        onSetValue=((array, undoArray) => send(SetValues(synthTrack.id, array, undoArray)))
        onSetLength=((index) => send(SetLoopAfterIndex(synthTrack.id, index)))
      />
      <div className="w1 flex-none" />
      <div className="flex flex-none">
        <button
          onClick=(_event => send(RandomiseAbsolute(synthTrack.id)))
        >
          (ReasonReact.string("Random Absolute"))
        </button>
        <button
          onClick=(_event => send(RandomiseRelative(synthTrack.id)))
        >
          (ReasonReact.string("Random Relative"))
        </button>
        <button
          onClick=(_event => send(Reset(synthTrack.id)))
        >
          (ReasonReact.string("Reset")
        )</button>
      </div>
    </div>
  }
};
