type synthTrack = {
  id: Id.t,
  label: string,
  values: SynthValues.values,
  valueConverter: SynthValues.valueConverter,
  loopLength: int,
  timing: Timing.t
};

let createSynthTrack = (globalParameters, (label, valueConverter)) => {
  let values = SynthValues.defaultValues(16, globalParameters, valueConverter);

  {
    id: Id.create(),
    label,
    values,
    valueConverter,
    loopLength: 8,
    timing: Timing.create(1)
  };
};

let defaultSynthTracks = (globalParameters) => {
  open SynthValues;
  open SynthValuesHelpers;

  [
    ("Octave", createValueConverter(octave)),
    ("Pitch 1", createValueConverter(pitchWithFirstNote)),
    ("Pitch 2", createValueConverter(pitch)),
    ("Pitch 3", createValueConverter(pitch)),
    ("Gain", createValueConverter(gain)),
    ("Pan", createValueConverter(pan)),
    ("Chance", createValueConverter(chance)),
    ("Length", createValueConverter(length)),
    ("Filter", createValueConverter(filter))
  ] |> List.map(createSynthTrack(globalParameters));
};

let mergeSynthTracks = (incomingSynthTracks, existingSynthTracks) => {
  let existingSynthTracksCurrent = ref(existingSynthTracks);

  List.map(incomingSynthTrack => {
    switch (existingSynthTracksCurrent^) {
      | [existingSynthTrack, ...existingSynthTracksRest] => {
        existingSynthTracksCurrent := existingSynthTracksRest;

        {
          ...incomingSynthTrack,
          timing: Timing.merge(incomingSynthTrack.loopLength, incomingSynthTrack.timing, existingSynthTrack.timing)
        };
      }
      | [] => incomingSynthTrack;
    }
  }, incomingSynthTracks);
};

let mapSynthTrackById = (id, fn, synthTracks) => {
  List.map((synthTrack) => {
    if (Id.equals(synthTrack.id, id)) {
      fn(synthTrack);
    } else {
      synthTrack;
    }
  }, synthTracks);
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
  globalTranspose: int,
  playbackSideEffects: ref(array((state, WebAudio.scheduleTime)))
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
    globalTranspose: randomTranspose(),
    playbackSideEffects: ref([||])
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
      synthTracks: List.map(synthTrack => {
        ...synthTrack,
        timing: Timing.restart(synthTrack.timing)
      }, state.synthTracks),
      tick: 0
    }
    | AdvancePlayback(scheduleTime) => {
      let nextTick = state.tick + 1;
      let sync = state.sync ? Timing.Sync(nextTick) : Timing.NoSync;

      let playbackSideEffects = Array.append(state.playbackSideEffects^, [|(state, scheduleTime)|]);

      {
        ...state,
        synthTracks: List.map((synthTrack) => {
          ...synthTrack,
          timing: Timing.advance(synthTrack.loopLength, sync, synthTrack.timing),
        }, state.synthTracks),
        tick: nextTick,
        playbackSideEffects: ref(playbackSideEffects)
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
        ...synthTrack,
        values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values),
        loopLength: Utils.randomInt(2, SynthValues.valuesLength(synthTrack.values))
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
      synthTracks: mapSynthTrackById(id, (synthTrack) => {
        ...synthTrack,
        loopLength: index
      }, state.synthTracks)
    }
    | RandomiseAbsolute(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values),
        loopLength: Utils.randomInt(2, SynthValues.valuesLength(synthTrack.values))
      }, state.synthTracks)
    }
    | RandomiseRelative(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.randomValuesRelative(state.globalParameters, synthTrack.valueConverter, synthTrack.values)
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
      synthTracks: mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.defaultValues(SynthValues.valuesLength(synthTrack.values), state.globalParameters, synthTrack.valueConverter)
      }, state.synthTracks)
    }
    | SetSubTicks(id, subTicks) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        timing: Timing.setSubTicks(subTicks, synthTrack.timing)
      }, state.synthTracks)
    }
    | TrackEditMode(id, update, action) => {
      let targetSynthTrack = List.find(synthTrack => synthTrack.id === id, state.synthTracks);

      let (newEditMode, sideEffects, valuesToUndo) = TrackEditMode.updateEditMode(id, targetSynthTrack.values, update, action, state.editMode);

      let applyUpdate = (id, values, update) => {
        mapSynthTrackById(id, (synthTrack) => {
          ...synthTrack,
          values: SynthValues.updateValues(state.globalParameters, synthTrack.valueConverter, values, update)
        });
      };

      let restoreValues = (id, values) => {
        mapSynthTrackById(id, (synthTrack) => {
          ...synthTrack,
          values
        });
      };

      let valuesToUpdate = switch (sideEffects) {
        | NoSideEffects => None
        | ApplyUpdateToValues(id, values, update) => Some(applyUpdate(id, values, update))
        | RestoreValues(id, values) => Some(restoreValues(id, values))
      };

      {
        ...state,
        editMode: newEditMode,
        synthTracks: switch (valuesToUpdate) {
          | None => state.synthTracks
          | Some(fn) => fn(state.synthTracks)
        },
        synthTracksUndoBuffer: switch (valuesToUndo) {
          | None => state.synthTracksUndoBuffer
          | Some((id, values)) => {
            let synthTracks = mapSynthTrackById(id, (synthTrack) => {
              ...synthTrack,
              values
            }, state.synthTracks)

            UndoBuffer.write(synthTracks, state.synthTracksUndoBuffer);
          }
        }
      };
    }
  }
};

let scheduleCallback = (state, beatTime, beatLength) => {
  let initialParameters = SynthParameters.{
    chance: 1.0,
    filter: 1.0,
    gain: 1.0,
    length: 1.0,
    notes: [||],
    pan: 0.0,
    transpose: 0
  };

  let playback = List.fold_left((parameters, synthTrack) => {
    SynthValues.updateSynthParameters(state.globalParameters, parameters, synthTrack.timing, synthTrack.values, synthTrack.valueConverter);
  }, initialParameters, state.synthTracks);

  if (playback.chance > 0.0) {
    let offset = if (state.tick mod 2 === 1) {
      (state.warble ** 2.3) *. 0.3;
    } else {
      0.0;
    };

    // avoid duplicate notes by sorting in ascending order then skipping over
    // values <= the previous value.
    Array.sort((a, b) => a - b, playback.notes);

    let lastNotePlayed = ref(-1);

    Array.iter((incomingNote) => {
      if ((incomingNote > lastNotePlayed^) && (Random.float(1.) <= playback.chance)) {
        lastNotePlayed := incomingNote;

        WebAudio.playSynth(
          ~note=incomingNote + playback.transpose + state.globalTranspose,
          ~gain=playback.gain,
          ~pan=playback.pan,
          ~filter=playback.filter,
          ~start=beatTime +. (beatLength *. offset),
          ~time=beatLength *. playback.length
        );
      }
    }, playback.notes);
  };

  WebAudio.playHihat(~start=beatTime);
};

let useScheduler = (state, dispatch) => {
  let schedulerRef = React.useRef(None);
  let beatTimeOffsetRef = React.useRef(None);

  let scheduler = switch (React.Ref.current(schedulerRef)) {
    | Some(scheduler) => scheduler;
    | None => {
      let scheduler = WebAudio.createSchedule((scheduleTime) => {
        dispatch(Actions.AdvancePlayback(scheduleTime));
      });

      React.Ref.setCurrent(schedulerRef, Some(scheduler));

      scheduler;
    }
  };

  scheduler.setBpm(state.bpm);

  React.useEffect1(() => {
    switch (state.playbackSideEffects^) {
      | [||] => ()
      | playbackSideEffects => {
        state.playbackSideEffects := [||];

        // we have two timings to deal with:
        // - the time the action was dispatched via the scheduler.
        // - the time this effect is executed.
        //
        // for example:
        // - the scheduler starts 10 seconds in. it dispatches two actions –
        //   play a note at 10.0 seconds (immediately) and 10.4 seconds.
        // - the effect executes for the first time at 10.2 seconds.
        //
        // this means the effect is behind - it can't play the note at
        // 10.0 seconds as it's already 10.2. so the first note is brought
        // forward to 10.2 seconds, but the second note can be played at 10.4,
        // as it's still in the future. they're played 0.2 seconds apart, when
        // they should be 0.4 seconds apart, throwing off the timing.
        //
        // to work around this, we can calculate the difference between the
        // scheduler's idea of "now" and this effect's idea of "now". we know
        // the first beat's timing is always intended to be immediate, so the
        // effect can subtract this time from the current audio clock time:
        //
        // - it's now 10.2, the first beat was meant to be 10.0, so we're
        //   0.2 seconds behind.
        // - we play the first note at 10.0 + 0.2 (10.2, i.e. immediately).
        // - we play the second note at 10.4 + 0.2 (10.6).
        //
        // if all subsequent notes are played with this offset, the timing
        // between notes is retained.

        let beatTimeOffset = switch (React.Ref.current(beatTimeOffsetRef)) {
          | None => {
            let (_, scheduleTime) = playbackSideEffects[0];
            let beatTimeOffset = WebAudio.getCurrentTime(WebAudio.audioContext) -. scheduleTime.beatTime;

            React.Ref.setCurrent(beatTimeOffsetRef, Some(beatTimeOffset));

            beatTimeOffset;
          }
          | Some(beatTimeOffset) => beatTimeOffset
        };

        Array.iter(((state, scheduleTime:WebAudio.scheduleTime)) => {
          scheduleCallback(state, scheduleTime.beatTime +. beatTimeOffset, scheduleTime.beatLength);
        }, playbackSideEffects);
      }
    };

    None;
  }, [|state.playbackSideEffects|]);

  React.useEffect2(() => {
    switch (state.isPlaying) {
      | true => {
        React.Ref.setCurrent(beatTimeOffsetRef, None);

        dispatch(Restart);

        scheduler.start();

        Some(() => {
          scheduler.stop();
        });
      }
      | false => {
        scheduler.stop();

        None;
      }
    };
  }, (dispatch, state.isPlaying));
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducerWithMapState(reducer, (), initialState);

  useScheduler(state, dispatch);

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
        <React.Fragment key=(Id.toString(synthTrack.id))>
          {index > 0 ? <span className="dib h1 flex-none" /> : React.null}
          <Track
            id=synthTrack.id
            label=synthTrack.label
            valueConverter=synthTrack.valueConverter
            values=synthTrack.values
            loopLength=synthTrack.loopLength
            timing=synthTrack.timing
            editMode=state.editMode
            globalParameters=state.globalParameters
            dispatch
          />
        </React.Fragment>
      }, state.synthTracks)
      |> Array.of_list
      |> React.array
    )
  </div>
};
