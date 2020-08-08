type parameter =
  | Octave
  | PitchWithNotes
  | PitchWithoutNotes
  | Gain
  | Pan
  | Chance
  | Length
  | Filter;

type synthTrack = {
  id: Id.t,
  label: string,
  parameter,
  synthInstance: SynthInstance.t
};

let getValueConverter = (globalParameters: SynthParameters.globalParameters, parameter) => {
  open SynthValues;
  open SynthValuesHelpers;

  switch (parameter) {
    | Octave => createValueConverter(octave)
    | PitchWithNotes => createValueConverter(pitchWithFirstNote(globalParameters.scale));
    | PitchWithoutNotes => createValueConverter(pitch(globalParameters.scale))
    | Gain => createValueConverter(gain)
    | Pan => createValueConverter(pan)
    | Chance => createValueConverter(chance)
    | Length => createValueConverter(SynthValuesHelpers.length)
    | Filter => createValueConverter(filter)
  };
};

let defaultSynthTracks = (globalParameters) => {
  [
    ("Octave", Octave),
    ("Pitch 1", PitchWithNotes),
    ("Pitch 2", PitchWithoutNotes),
    ("Pitch 3", PitchWithoutNotes),
    ("Gain", Gain),
    ("Pan", Pan),
    ("Chance", Chance),
    ("Length", Length),
    ("Filter", Filter),
  ] |> List.map(((label, parameter)) => {
    let valueConverter = getValueConverter(globalParameters, parameter);
    let values = valueConverter.defaultValues(16);

    {
      id: Id.create(),
      label,
      parameter,
      synthInstance: SynthInstance.create(values)
    };
  });
};

let mergeSynthTracks = (incomingSynthTracks, existingSynthTracks) => {
  let existingSynthTracksCurrent = ref(existingSynthTracks);

  List.map(incomingSynthTrack => {
    switch (existingSynthTracksCurrent^) {
      | [existingSynthTrack, ...existingSynthTracksRest] => {
        existingSynthTracksCurrent := existingSynthTracksRest;

        {
          ...incomingSynthTrack,
          synthInstance: SynthInstance.merge(incomingSynthTrack.synthInstance, existingSynthTrack.synthInstance)
        };
      }
      | [] => incomingSynthTrack;
    }
  }, incomingSynthTracks);
};

type state = {
  synthTracks: list(synthTrack),
  synthTracksUndoBuffer: UndoBuffer.t(list(synthTrack)),
  synthTracksRedoBuffer: UndoBuffer.t(list(synthTrack)),
  isPlaying: bool,
  volume: float,
  warble: float,
  bpm: float,
  tick: int,
  sync: bool,
  globalParameters: SynthParameters.globalParameters,
  editMode: TrackEditMode.editMode(SynthValues.values),
  globalTranspose: int
};

let updateSynthTrackById = (id, fn, synthTracks) => {
  List.map((synthTrack) => {
    if (Id.equals(synthTrack.id, id)) {
      fn(synthTrack);
    } else {
      synthTrack;
    }
  }, synthTracks);
};

let updateSynthInstance = (fn, synthTrack) => {
  ...synthTrack,
  synthInstance: fn(synthTrack.synthInstance)
};

let randomTranspose = () => Utils.randomInt(-5, 6);

let initialState = () => {
  let (_, initialScale) = Utils.randomArrayValue(Scales.scales);

  let initialGlobalParameters = SynthParameters.{
    repeatNotesEverySubTick: false,
    scale: initialScale
  };

  {
    synthTracks: defaultSynthTracks(initialGlobalParameters),
    synthTracksUndoBuffer: UndoBuffer.create(12, []),
    synthTracksRedoBuffer: UndoBuffer.create(12, []),
    isPlaying: false,
    volume: 1.0,
    warble: 0.0,
    bpm: 120.0,
    tick: 0,
    sync: false,
    globalParameters: initialGlobalParameters,
    editMode: Inactive,
    globalTranspose: randomTranspose()
  };
};

let reducer = (state, action) => {
  switch (action:Actions.action) {
    | Undo => switch (UndoBuffer.read(state.synthTracksUndoBuffer)) {
      | None => state
      | Some(synthTracks) => {
        ...state,
        synthTracks: mergeSynthTracks(synthTracks, state.synthTracks),
        synthTracksUndoBuffer: UndoBuffer.pop(state.synthTracksUndoBuffer),
        synthTracksRedoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksRedoBuffer)
      }
    }
    | Redo => switch (UndoBuffer.read(state.synthTracksRedoBuffer)) {
      | None => state
      | Some(synthTracks) => {
        ...state,
        synthTracks: mergeSynthTracks(synthTracks, state.synthTracks),
        synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
        synthTracksRedoBuffer: UndoBuffer.pop(state.synthTracksRedoBuffer)
      }
    }
    | Restart => {
      ...state,
      synthTracks: List.map(SynthInstance.restart |> updateSynthInstance, state.synthTracks),
      tick: 0
    }
    | AdvancePlayback => {
      let nextTick = state.tick + 1;
      let sync = state.sync ? Timing.Sync(nextTick) : Timing.NoSync;

      {
        ...state,
        synthTracks: List.map(SynthInstance.advance(sync) |> updateSynthInstance, state.synthTracks),
        tick: nextTick
      };
    }
    | SetPlayback(value) => {
      ...state,
      isPlaying: value
    }
    | SetSync(value) => {
      ...state,
      sync: value
    }
    | RandomiseAll => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: List.map((synthTrack) => {
        let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

        {
          ...synthTrack,
          synthInstance: SynthInstance.randomAbsolute(valueConverter, synthTrack.synthInstance)
        };
      }, state.synthTracks),
      globalTranspose: randomTranspose()
    }
    | UpdateGlobalParameters(globalParameters) => {
      ...state,
      globalParameters
    }
    | SetVolume(volume) => {
      ...state,
      volume
    }
    | SetWarble(warble) => {
      ...state,
      warble
    }
    | SetBpm(bpm) => {
      ...state,
      bpm
    }
    | SetLoopLength(id, index) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: updateSynthTrackById(id, SynthInstance.setLoopLength(index) |> updateSynthInstance, state.synthTracks),
    }
    | RandomiseAbsolute(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: updateSynthTrackById(id, (synthTrack) => {
        let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

        {
          ...synthTrack,
          synthInstance: SynthInstance.randomAbsolute(valueConverter, synthTrack.synthInstance)
        };
      }, state.synthTracks),
    }
    | RandomiseRelative(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: updateSynthTrackById(id, (synthTrack) => {
        let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

        {
          ...synthTrack,
          synthInstance: SynthInstance.randomRelative(valueConverter, synthTrack.synthInstance)
        };
      }, state.synthTracks)
    }
    | ResetAll => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: defaultSynthTracks(state.globalParameters)
    }
    | Reset(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: updateSynthTrackById(id, (synthTrack) => {
        let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

        {
          ...synthTrack,
          synthInstance: SynthInstance.reset(valueConverter, synthTrack.synthInstance)
        };
      }, state.synthTracks),
    }
    | SetSubTicks(id, subTicks) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: updateSynthTrackById(id, SynthInstance.setSubTicks(subTicks) |> updateSynthInstance, state.synthTracks)
    }
    | TrackEditMode(id, update, action) => {
      let selectedSynthTrack = List.find((synthTrack) => synthTrack.id === id, state.synthTracks);
      let selectedValues = SynthInstance.values(selectedSynthTrack.synthInstance);

      let (newEditMode, sideEffects, valuesToUndo) = TrackEditMode.updateEditMode(id, selectedValues, update, action, state.editMode);

      let synthTracks = switch (sideEffects) {
        | NoSideEffects => state.synthTracks
        | ApplyUpdateToValues(id, values, update) => {
          updateSynthTrackById(id, (synthTrack) => {
            let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

            {
              ...synthTrack,
              synthInstance: SynthInstance.applyUpdate(valueConverter, values, update, synthTrack.synthInstance)
            };
          }, state.synthTracks);
        }
        | RestoreValues(id, values) => {
          updateSynthTrackById(id, SynthInstance.setValues(values) |> updateSynthInstance, state.synthTracks);
        }
      };

      let synthTracksUndoBuffer = switch (valuesToUndo) {
        | None => state.synthTracksUndoBuffer
        | Some((id, values)) => {
          let synthTracks = updateSynthTrackById(id, SynthInstance.setValues(values) |> updateSynthInstance, state.synthTracks)

          UndoBuffer.write(synthTracks, state.synthTracksUndoBuffer);
        }
      };

      {
        ...state,
        editMode: newEditMode,
        synthTracks,
        synthTracksUndoBuffer
      };
    }
  }
};

let scheduleCallback = (getState, beatTime, beatLength) => {
  let initialParameters = SynthParameters.{
    chance: 1.0,
    filter: 1.0,
    gain: 1.0,
    length: 1.0,
    notes: [||],
    pan: 0.0,
    transpose: 0
  };

  let state = getState();

  let parameters = List.fold_left((parameters, synthTrack) => {
    let synthInstance = synthTrack.synthInstance;
    let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

    let group = {
      SynthValues.globalParameters: state.globalParameters,
      SynthValues.timing: SynthInstance.timing(synthInstance)
    };

    valueConverter.updateSynthParameters(group, parameters, SynthInstance.values(synthInstance));
  }, initialParameters, state.synthTracks);

  if (parameters.chance > 0.0) {
    let offset = if (state.tick mod 2 === 1) {
      (state.warble ** 2.3) *. 0.3;
    } else {
      0.0;
    };

    // avoid duplicate notes by sorting in ascending order then skipping over
    // values <= the previous value.
    Array.sort((a, b) => a - b, parameters.notes);

    let lastNotePlayed = ref(-1);

    Array.iter((incomingNote) => {
      if ((incomingNote > lastNotePlayed^) && (Random.float(1.) <= parameters.chance)) {
        lastNotePlayed := incomingNote;

        WebAudio.playSynth(
          ~note=incomingNote + parameters.transpose + state.globalTranspose,
          ~gain=parameters.gain,
          ~pan=parameters.pan,
          ~filter=parameters.filter,
          ~start=beatTime +. (beatLength *. offset),
          ~time=beatLength *. parameters.length
        );
      }
    }, parameters.notes);
  };

  WebAudio.playHihat(~start=beatTime);
};

let useReducerRealTime = (reducer, initialState) => {
  let (state, setState) = React.useState(initialState);
  let stateRef = React.useRef(state);

  let getState = React.useCallback1(() => {
    stateRef.current;
  }, [||]);

  let dispatch = React.useCallback1((action) => {
    let previousState = getState();
    let nextState = reducer(previousState, action);

    stateRef.current = nextState;

    setState((_) => nextState);
  }, [|reducer|]);

  (state, dispatch, getState);
};

let useScheduler = ((state, dispatch, getState)) => {
  let schedulerRef = React.useRef(None);

  let scheduler = switch (schedulerRef.current) {
    | Some(scheduler) => scheduler;
    | None => {
      let scheduler = WebAudio.createSchedule((scheduleTime) => {
        scheduleCallback(getState, scheduleTime.beatTime, scheduleTime.beatLength);

        dispatch(Actions.AdvancePlayback);
      });

      schedulerRef.current = Some(scheduler);

      scheduler;
    }
  };

  // make sure scheduler is stopped on unmount.
  React.useEffect1(() => Some(() => scheduler.stop()), [||]);

  let dispatchWrapper = React.useCallback1((action) => {
    let previousState = getState();

    dispatch(action);

    let nextState = getState();

    scheduler.setBpm(nextState.bpm);

    switch (previousState.isPlaying, nextState.isPlaying) {
      | (false, true) => scheduler.start()
      | (true, false) => scheduler.stop()
      | _ => ()
    };
  }, [||]);

  (state, dispatchWrapper);
};

[@react.component]
let make = () => {
  let (state, dispatch) = useReducerRealTime(reducer, initialState)
    |> useScheduler;

  React.useEffect1(() => {
    WebAudio.setGlobalVolume(state.volume);

    None;
  }, [|state.volume|]);

  React.useEffect1(() => {
    WebAudio.setGlobalWarble(state.warble *. 2.9);

    None;
  }, [|state.warble|]);

  <div className="ma4">
    <div className="flex items-center f6">
      <button className="w3 h2 flex-none" onClick=(_event => {
        WebAudio.resume();

        dispatch(SetPlayback(!state.isPlaying));
      })>
        (state.isPlaying ? React.string("Stop") : React.string("Play"))
      </button>
      <span className="db w1 flex-none" />
      <Range
        value=state.bpm
        min=40.0
        max=200.0
        step=1.0
        onChange=(value => dispatch(SetBpm(value)))
      >
        <span>(React.string("BPM: " ++ Js.Float.toString(state.bpm)))</span>
      </Range>
      <span className="db w1 flex-none" />
      <Range
        value=state.volume
        min=0.0
        max=1.0
        step=0.01
        onChange=(value => dispatch(SetVolume(value)))
      >
        (React.string("Volume: " ++ (Js.Math.floor(state.volume *. 100.0) |> string_of_int) ++ "%"))
      </Range>
      <span className="db w1 flex-none" />
      <Range
        value=state.warble
        min=0.0
        max=1.0
        step=0.01
        onChange=(value => dispatch(SetWarble(value)))
      >
        (React.string("Warble: " ++ (Js.Math.floor(state.warble *. 100.0) |> string_of_int) ++ "%"))
      </Range>
      <span className="db w2 flex-none" />
      <button className="w4 h2 flex-none" onClick=(_event => dispatch(Restart))>
        (React.string("Restart All"))
      </button>
      <span className="db w1 flex-none" />
      <button className="w4 h2 flex-none" onClick=(_event => dispatch(RandomiseAll))>
        (React.string("Randomise All"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="w4 h2 flex-none"
        onClick=(_event => dispatch(ResetAll))
      >
        (React.string("Reset All"))
      </button>
      <span className="db w2 flex-none" />
      <button
        className="w3 h2 flex-none"
        disabled=(UndoBuffer.isEmpty(state.synthTracksUndoBuffer))
        onClick=(_event => dispatch(Undo))
      >
        (React.string("Undo"))
      </button>
      <span className="db w1 flex-none" />
      <button
        className="w3 h2 flex-none"
        disabled=(UndoBuffer.isEmpty(state.synthTracksRedoBuffer))
        onClick=(_event => dispatch(Redo))
      >
        (React.string("Redo"))
      </button>
      <span className="db w2 flex-none" />
      <label className="flex-none">
        <input type_="checkbox" checked=state.sync onChange=(event => {
          dispatch(SetSync(event->ReactEvent.Form.target##checked));
        }) />
        <span className="ml2">(React.string("Sync"))</span>
      </label>
      <span className="db w1 flex-none" />
      <label className="flex-none">
        <input
          type_="checkbox"
          checked=state.globalParameters.repeatNotesEverySubTick
          onChange=((event) => dispatch(UpdateGlobalParameters({
            ...state.globalParameters,
            repeatNotesEverySubTick: event->ReactEvent.Form.target##checked
          })))
        />
        <span className="ml2">(React.string("Repeat notes"))</span>
      </label>
    </div>
    <span className="dib h2" />
    <div className="flex">
      <p className="ma0">(React.string("Scale:"))</p>
      (Array.map(((label, scale)) =>
        <label key=label className="ml4">
          <input
            type_="radio"
            name="scale"
            value=label
            checked=(scale === state.globalParameters.scale)
            onChange=((_event) => dispatch(UpdateGlobalParameters({
              ...state.globalParameters,
              scale
            })))
          />
          <span className="ml2">(React.string(label))</span>
        </label>
      , Scales.scales)
      |> React.array)
    </div>
    <span className="dib h2" />
    (
      List.mapi((index, synthTrack) => {
        let valueConverter = getValueConverter(state.globalParameters, synthTrack.parameter);

        <React.Fragment key=(Id.toString(synthTrack.id))>
          {index > 0 ? <span className="dib h1 flex-none" /> : React.null}
          <Track
            id=synthTrack.id
            label=synthTrack.label
            valueConverter=valueConverter
            synthInstance=synthTrack.synthInstance
            editMode=state.editMode
            dispatch
          />
        </React.Fragment>
      }, state.synthTracks)
      |> Array.of_list
      |> React.array
    )
  </div>
};
