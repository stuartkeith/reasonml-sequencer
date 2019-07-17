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

  let mapValues = React.useCallback2(
    SynthValues.mapValues(globalParameters, synthTrack.valueConverter),
    (globalParameters, synthTrack.valueConverter)
  );

  let getValueAt = React.useCallback2(
    SynthValues.getValueAt(globalParameters, synthTrack.valueConverter),
    (globalParameters, synthTrack.valueConverter)
  );

  let onAction = React.useCallback1(
    (update, action) => dispatch(Actions.TrackEditMode(synthTrack.id, update, action)),
    [|synthTrack.id|]
  );

  let onSetLength = React.useCallback1(
    (index) => dispatch(Actions.SetLoopLength(synthTrack.id, index)),
    [|synthTrack.id|]
  );

  <div className="flex items-center">
    <p className="ma0 w4 flex-none">(React.string(synthTrack.label))</p>
    <div className="w1 flex-none" />
      <select
        value=(synthTrack.timing |> Timing.subTicks |> string_of_int)
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
      mapValues
      getValueAt
      values=synthTrack.values
      highlightedIndex=Timing.index(synthTrack.timing)
      disabledIndex=synthTrack.loopLength
      onAction
      onSetLength
    />
    <div className="w1 flex-none" />
    <div className="flex flex-none f6">
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(RandomiseAbsolute(synthTrack.id)))
      >
        (React.string("Random Absolute"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(RandomiseRelative(synthTrack.id)))
      >
        (React.string("Random Relative"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(Reset(synthTrack.id)))
      >
        (React.string("Reset")
      )</button>
    </div>
  </div>
};
