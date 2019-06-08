let options = Array.init(64, i => (string_of_int(i + 1), i + 1));

[@react.component]
let make = (~synthTrack:SynthTrack.t, ~editMode:TrackEditMode.editMode, ~globalParameters:SynthParameters.globalParameters, ~dispatch) => {
  let viewMode = switch (editMode) {
    | Inactive => Slider.Inactive
    | Preview(id, values, index) when id === synthTrack.id => Slider.Preview(values, index)
    | Preview(_) => Slider.Deactive
    | Active(id, _, _) when id === synthTrack.id => Slider.Active
    | Active(_) => Slider.Deactive
  };

  <div className="flex items-center">
    <p className="ma0 w4 flex-none">(React.string(synthTrack.label))</p>
    <div className="w1 flex-none" />
      <select
        value=(string_of_int(synthTrack.subTicks))
        onChange=((event) => {
          let value = int_of_string(event->ReactEvent.Form.target##value);

          dispatch(Actions.SetSubTicks(synthTrack.id, value));
        })
      >
        (Array.map(((label, _value)) => {
          <option key=label value=label>(React.string(label))</option>
        }, options)
        |> React.array)
      </select>
    <div className="w1 flex-none" />
    <Slider
      viewMode
      mapValues=SynthValues.mapValues(globalParameters, synthTrack.valueConverter)
      getValuesAt=SynthValues.getValuesAt(globalParameters, synthTrack.valueConverter)
      values=synthTrack.values
      highlightedIndex=Timing.index(synthTrack.loopAfterIndex, synthTrack.timing)
      disabledAfterIndex=synthTrack.loopAfterIndex
      onAction=((update, action) => dispatch(TrackEditMode(synthTrack.id, update, action)))
      onSetLength=((index) => dispatch(SetLoopAfterIndex(synthTrack.id, index)))
    />
    <div className="w1 flex-none" />
    <div className="flex flex-none">
      <button
        onClick=(_event => dispatch(RandomiseAbsolute(synthTrack.id)))
      >
        (React.string("Random Absolute"))
      </button>
      <button
        onClick=(_event => dispatch(RandomiseRelative(synthTrack.id)))
      >
        (React.string("Random Relative"))
      </button>
      <button
        onClick=(_event => dispatch(Reset(synthTrack.id)))
      >
        (React.string("Reset")
      )</button>
    </div>
  </div>
};
