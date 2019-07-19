let options = Array.init(64, i => (string_of_int(i + 1), i + 1));

[@react.component]
let make = (
  ~id: Id.t,
  ~label: string,
  ~valueConverter: SynthValues.valueConverter,
  ~values: SynthValues.values,
  ~loopLength: int,
  ~timing: Timing.t,
  ~editMode: TrackEditMode.editMode(SynthValues.values),
  ~globalParameters: SynthParameters.globalParameters,
  ~dispatch
) => {
  let viewMode = switch (editMode) {
    | Inactive => Slider.Inactive
    | Preview(preview) when preview.id === id => Slider.Preview(preview.valuesBeforeEdit, preview.index)
    | Preview(_) => Slider.Deactive
    | Active(active) when active.id === id => Slider.Active
    | Active(_) => Slider.Deactive
  };

  let mapValues = React.useCallback2(
    SynthValues.mapValues(globalParameters, valueConverter),
    (globalParameters, valueConverter)
  );

  let getValueAt = React.useCallback2(
    SynthValues.getValueAt(globalParameters, valueConverter),
    (globalParameters, valueConverter)
  );

  let onAction = React.useCallback1(
    (update, action) => dispatch(Actions.TrackEditMode(id, update, action)),
    [|id|]
  );

  let onSetLength = React.useCallback1(
    (index) => dispatch(Actions.SetLoopLength(id, index)),
    [|id|]
  );

  <div className="flex items-center">
    <p className="ma0 w4 flex-none">(React.string(label))</p>
    <div className="w1 flex-none" />
      <select
        value=(timing |> Timing.subTicks |> string_of_int)
        onChange=((event) => {
          let value = int_of_string(event->ReactEvent.Form.target##value);

          dispatch(Actions.SetSubTicks(id, value));
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
      values
      highlightedIndex=Timing.index(timing)
      disabledIndex=loopLength
      onAction
      onSetLength
    />
    <div className="w1 flex-none" />
    <div className="flex flex-none f6">
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(RandomiseAbsolute(id)))
      >
        (React.string("Random Absolute"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(RandomiseRelative(id)))
      >
        (React.string("Random Relative"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="flex-none h2"
        onClick=(_event => dispatch(Reset(id)))
      >
        (React.string("Reset")
      )</button>
    </div>
  </div>
};
