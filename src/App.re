type state = {
  synthTracks: list(SynthTrack.t),
  synthTracksUndoBuffer: UndoBuffer.t(list(SynthTrack.t)),
  synthTracksRedoBuffer: UndoBuffer.t(list(SynthTrack.t)),
  isPlaying: bool,
  volume: float,
  bpm: int,
  tick: int,
  sync: bool,
  globalParameters: SynthParameters.globalParameters,
  editMode: TrackEditMode.editMode
};

let initialState = () => {
  let (_, initialScale) = Utils.randomArrayValue(Scales.scales);

  let initialGlobalParameters = SynthParameters.{
    scale: initialScale
  };

  let synthTracks = SynthTracks.default(initialGlobalParameters);

  {
    synthTracks,
    synthTracksUndoBuffer: UndoBuffer.create(12, []),
    synthTracksRedoBuffer: UndoBuffer.create(12, []),
    isPlaying: false,
    volume: 1.0,
    bpm: 120,
    tick: 0,
    sync: false,
    globalParameters: initialGlobalParameters,
    editMode: Inactive
  };
};

let reducer = (state, action) => {
  switch (action:Actions.action) {
    | Undo => switch (UndoBuffer.read(state.synthTracksUndoBuffer)) {
      | None => state
      | Some(synthTracks) => {
        ...state,
        synthTracks: SynthTracks.merge(synthTracks, state.synthTracks),
        synthTracksUndoBuffer: UndoBuffer.pop(state.synthTracksUndoBuffer),
        synthTracksRedoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksRedoBuffer)
      }
    }
    | Redo => switch (UndoBuffer.read(state.synthTracksRedoBuffer)) {
      | None => state
      | Some(synthTracks) => {
        ...state,
        synthTracks: SynthTracks.merge(synthTracks, state.synthTracks),
        synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
        synthTracksRedoBuffer: UndoBuffer.pop(state.synthTracksRedoBuffer)
      }
    }
    | Restart => {
      ...state,
      synthTracks: List.map(synthTrack:SynthTrack.t => {
        ...synthTrack,
        timing: Timing.default
      }, state.synthTracks),
      tick: 0
    }
    | AdvancePlayback => {
      let nextTick = state.tick + 1;
      let sync = state.sync ? Timing.Sync(nextTick) : Timing.NoSync;

      {
        ...state,
        synthTracks: List.map((synthTrack:SynthTrack.t) => {
          ...synthTrack,
          timing: Timing.advance(synthTrack.subTicks, synthTrack.loopAfterIndex, sync, synthTrack.timing),
        }, state.synthTracks),
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
    | RandomiseAll(canUndo) => {
      ...state,
      synthTracksUndoBuffer: canUndo ? UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer) : state.synthTracksUndoBuffer,
      synthTracks: List.map((synthTrack:SynthTrack.t) => {
        ...synthTrack,
        values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values),
        loopAfterIndex: Random.int(SynthValues.valuesLength(synthTrack.values))
      }, state.synthTracks)
    }
    | SetScale(scale) => {
      ...state,
      globalParameters: {
        scale: scale
      }
    }
    | SetVolume(volume) => {
      ...state,
      volume
    }
    | SetBpm(bpm) => {
      ...state,
      bpm
    }
    | SetLoopAfterIndex(id, index) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: SynthTracks.mapSynthTrackById(id, (synthTrack) => {
        ...synthTrack,
        loopAfterIndex: index
      }, state.synthTracks)
    }
    | RandomiseAbsolute(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values)
      }, state.synthTracks)
    }
    | RandomiseRelative(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.randomValuesRelative(state.globalParameters, synthTrack.valueConverter, synthTrack.values)
      }, state.synthTracks)
    }
    | Reset(id) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        values: SynthValues.defaultValues(state.globalParameters, synthTrack.valueConverter, SynthValues.valuesLength(synthTrack.values))
      }, state.synthTracks)
    }
    | SetSubTicks(id, subTicks) => {
      ...state,
      synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
      synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
        ...synthTrack,
        subTicks
      }, state.synthTracks)
    }
    | TrackEditMode(id, update, action) => {
      let applyUpdate = (synthTrack:SynthTrack.t, values, update:SynthValues.valuesUpdate) => {
        SynthValues.updateValues(state.globalParameters, synthTrack.valueConverter, values, update.index, update.value);
      };

      let targetSynthTrack = List.find(synthTrack => synthTrack.SynthTrack.id === id, state.synthTracks);

      switch (state.editMode, action) {
        | (Inactive, MouseEnter) => {
          ...state,
          // on enter, store the initial values for later.
          editMode: Preview(id, targetSynthTrack.values, update.index),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: applyUpdate(synthTrack, synthTrack.values, update)
          }, state.synthTracks)
        }
        | (Inactive, MouseMove) => state
        | (Inactive, MouseLeave) => state
        | (Inactive, MouseDown) => state
        | (Inactive, MouseUp) => state
        | (Preview(_), MouseEnter) => state
        | (Preview(id, values, index), MouseMove) when index !== update.index => {
          ...state,
          editMode: Preview(id, values, update.index),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            // apply the change to the clean stored values - the synthTrack's
            // current values are dirty from the previous index's preview.
            values: applyUpdate(synthTrack, values, update)
          }, state.synthTracks)
        }
        | (Preview(_), MouseMove) => {
          ...state,
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: applyUpdate(synthTrack, synthTrack.values, update)
          }, state.synthTracks)
        }
        | (Preview(id, values, _), MouseLeave) => {
          ...state,
          editMode: Inactive,
          synthTracks: SynthTracks.mapSynthTrackById(id, (synthTrack) => {
            ...synthTrack,
            // restore to the initial values completely - all changes discarded.
            values
          }, state.synthTracks)
        }
        | (Preview(id, values, _), MouseDown) => {
          ...state,
          editMode: Active(id, values, Inside)
        }
        | (Preview(_), MouseUp) => state
        | (Active(id, values, _), MouseEnter) => {
          ...state,
          editMode: Active(id, values, Inside)
        }
        | (Active(_), MouseMove) => {
          ...state,
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: applyUpdate(synthTrack, synthTrack.values, update)
          }, state.synthTracks)
        }
        | (Active(id, values, _), MouseLeave) => {
          ...state,
          editMode: Active(id, values, Outside)
        }
        | (Active(_), MouseDown) => state
        | (Active(id, values, mousePosition), MouseUp) => {
          ...state,
          editMode: switch (mousePosition) {
            | Inside => Preview(id, targetSynthTrack.values, update.index)
            | Outside => Inactive
          },
          // apply the pre-edited values and store in undo.
          synthTracksUndoBuffer: UndoBuffer.write(SynthTracks.mapSynthTrackById(id, (synthTrack) => {
            ...synthTrack,
            values
          }, state.synthTracks), state.synthTracksUndoBuffer)
        }
      };
    }
  }
};

let useScheduler = (state, dispatch) => {
  let schedulerRef = React.useRef(None);
  let synthTracksRef = React.useRef(state.synthTracks);
  let globalParametersRef = React.useRef(state.globalParameters);

  React.Ref.setCurrent(synthTracksRef, state.synthTracks);
  React.Ref.setCurrent(globalParametersRef, state.globalParameters);

  let scheduler = switch (React.Ref.current(schedulerRef)) {
    | Some(scheduler) => scheduler;
    | None => {
      let scheduler = WebAudio.createSchedule((beatTime, beatLength) => {
        let globalParameters = React.Ref.current(globalParametersRef);
        let synthTracks = React.Ref.current(synthTracksRef);

        let initialParameters = SynthParameters.{
          chance: 1.0,
          chord: [||],
          filter: 1.0,
          gain: 1.0,
          length: 1.0,
          note: 0,
          offset: 0.0,
          pan: 0.0
        };

        let playback = List.fold_left((initialParameters, synthTrack:SynthTrack.t) => {
          let index = Timing.index(synthTrack.loopAfterIndex, synthTrack.timing);

          SynthValues.updateSynthParameters(globalParameters, initialParameters, index, synthTrack.values, synthTrack.valueConverter);
        }, initialParameters, synthTracks);

        let chance = Random.float(1.);

        if (playback.chance > 0.0 && chance <= playback.chance) {
          let note = playback.note;
          let chord = playback.chord;
          let gain = playback.gain;
          let pan = playback.pan;
          let length = playback.length;
          let filter = playback.filter;

          WebAudio.playSynth(~note, ~chord, ~gain=gain, ~pan, ~start=beatTime +. (beatLength *. playback.offset), ~time=beatLength *. length, ~filter);
        }

        WebAudio.playHihat(~start=beatTime +. (beatLength *. playback.offset));

        dispatch(Actions.AdvancePlayback);
      });

      React.Ref.setCurrent(schedulerRef, Some(scheduler));

      scheduler;
    }
  };

  scheduler.setBpm(float_of_int(state.bpm));

  // stop scheduler when component removed.
  React.useEffect1(() => {
    Some(() => {
      scheduler.stop();
    });
  }, [||]);

  scheduler;
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducerWithMapState(reducer, (), initialState);
  let scheduler = useScheduler(state, dispatch);

  React.useEffect2(() => {
    switch (state.isPlaying) {
      | true => {
        dispatch(Restart);

        scheduler.start();
      }
      | false => {
        scheduler.stop();
      }
    }

    None;
  }, (dispatch, state.isPlaying));

  React.useEffect1(() => {
    WebAudio.setGlobalVolume(state.volume);

    None;
  }, [|state.volume|]);

  <div className="ma4">
    <div className="flex items-center">
      <button
        className="w4 h2 flex-none"
        disabled=(UndoBuffer.isEmpty(state.synthTracksUndoBuffer))
        onClick=(_event => dispatch(Undo))
      >
        (React.string("Undo"))
      </button>
      <button
        className="w4 h2 flex-none"
        disabled=(UndoBuffer.isEmpty(state.synthTracksRedoBuffer))
        onClick=(_event => dispatch(Redo))
      >
        (React.string("Redo"))
      </button>
      <Range
        value=float_of_int(state.bpm)
        label=("BPM: " ++ string_of_int(state.bpm))
        min=40.0
        max=200.0
        step=1.0
        onChange=(value => dispatch(SetBpm(int_of_float(value))))
      />
      <Range
        value=state.volume
        label=("Volume: " ++ (Js.Math.floor(state.volume *. 100.0) |> string_of_int) ++ "%")
        min=0.0
        max=1.0
        step=0.01
        onChange=(value => dispatch(SetVolume(value)))
      />
      <button className="w4 h2 flex-none" onClick=(_event => {
        WebAudio.resume();

        dispatch(SetPlayback(!state.isPlaying));
      })>
        (state.isPlaying ? React.string("Stop") : React.string("Play"))
      </button>
      <button className="w4 h2 flex-none" onClick=(_event => dispatch(Restart))>
        (React.string("Restart"))
      </button>
      <button className="w4 h2 flex-none" onClick=(_event => dispatch(RandomiseAll(true)))>
        (React.string("Randomise All"))
      </button>
      <label className="flex-none">
        <input type_="checkbox" checked=state.sync onChange=(event => {
          dispatch(SetSync(event->ReactEvent.Form.target##checked));
        }) />
        (React.string("Sync"))
      </label>
    </div>
    <div>
      (Array.map(((label, scale)) =>
        <label key=label>
          <input
            type_="radio"
            name="scale"
            value=label
            checked=(scale === state.globalParameters.scale)
            onChange=((_event) => dispatch(SetScale(scale)))
          />
          (React.string(label))
        </label>
      , Scales.scales)
      |> React.array)
    </div>
    (
      List.mapi((index, synthTrack:SynthTrack.t) => {
        <React.Fragment key=(Id.toString(synthTrack.id))>
          {index > 0 ? <div className="h1 flex-none" /> : React.null}
          <Track
            synthTrack
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
