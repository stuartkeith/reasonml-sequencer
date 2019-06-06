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
  scheduler: ref(option(WebAudio.schedule))
};

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,

  initialState: () => {
    let (_, initialScale) = Scales.scales[Random.int(Array.length(Scales.scales))];

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
      scheduler: ref(None),
      globalParameters: initialGlobalParameters
    };
  },

  reducer: (action, state) =>
    switch (action) {
      | Undo => switch (UndoBuffer.read(state.synthTracksUndoBuffer)) {
        | None => ReasonReact.NoUpdate
        | Some(synthTracks) => ReasonReact.Update({
          ...state,
          synthTracks: SynthTracks.merge(synthTracks, state.synthTracks),
          synthTracksUndoBuffer: UndoBuffer.pop(state.synthTracksUndoBuffer),
          synthTracksRedoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksRedoBuffer)
        })
      }
      | Redo => switch (UndoBuffer.read(state.synthTracksRedoBuffer)) {
        | None => ReasonReact.NoUpdate
        | Some(synthTracks) => ReasonReact.Update({
          ...state,
          synthTracks: SynthTracks.merge(synthTracks, state.synthTracks),
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracksRedoBuffer: UndoBuffer.pop(state.synthTracksRedoBuffer)
        })
      }
      | Restart => ReasonReact.Update({
        ...state,
        synthTracks: List.map(synthTrack:SynthTrack.t => {
          ...synthTrack,
          timing: Timing.default
        }, state.synthTracks),
        tick: 0
      })
      | Playback(beatTime, beatLength) => ReasonReact.SideEffects((self) => {
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

          SynthValues.updateSynthParameters(self.state.globalParameters, initialParameters, index, synthTrack.values, synthTrack.valueConverter);
        }, initialParameters, state.synthTracks);

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

        self.send(AdvancePlayback);
      })
      | AdvancePlayback => {
        let nextTick = state.tick + 1;
        let sync = state.sync ? Timing.Sync(nextTick) : Timing.NoSync;

        ReasonReact.Update({
          ...state,
          synthTracks: List.map((synthTrack:SynthTrack.t) => {
            ...synthTrack,
            timing: Timing.advance(synthTrack.subTicks, synthTrack.loopAfterIndex, sync, synthTrack.timing),
          }, state.synthTracks),
          tick: nextTick
        });
      }
      | SetPlayback(value) => ReasonReact.UpdateWithSideEffects({
        ...state,
        isPlaying: value
      }, (_) => WebAudio.resume())
      | SetSync(value) => ReasonReact.Update({
        ...state,
        sync: value
      })
      | RandomiseAll(canUndo) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: canUndo ? UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer) : state.synthTracksUndoBuffer,
          synthTracks: List.map((synthTrack:SynthTrack.t) => {
            ...synthTrack,
          values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values),
          loopAfterIndex: Random.int(SynthValues.valuesLength(synthTrack.values))
          }, state.synthTracks)
        })
      }
      | SetScale(scale) => ReasonReact.Update({
        ...state,
        globalParameters: {
          scale: scale
        }
      })
      | SetVolume(volume) => ReasonReact.Update({
        ...state,
        volume
      })
      | SetBpm(bpm) => ReasonReact.Update({
        ...state,
        bpm
      })
      | SetValues(id, values, synthTracksToUndo) => {
        let synthTracksUndoBuffer = switch (synthTracksToUndo) {
          | Some(values) => {
            let synthTracks = SynthTracks.mapSynthTrackById(id, (synthTrack => {
              ...synthTrack,
              values
            }), state.synthTracks);

            UndoBuffer.write(synthTracks, state.synthTracksUndoBuffer);
          }
          | None => state.synthTracksUndoBuffer
        };

        let synthTracks = SynthTracks.mapSynthTrackById(id, (synthTrack => {
          ...synthTrack,
          values
        }), state.synthTracks);

        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer,
          synthTracks
        });
      }
      | SetLoopAfterIndex(id, index) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracks: SynthTracks.mapSynthTrackById(id, (synthTrack) => {
            ...synthTrack,
            loopAfterIndex: index
          }, state.synthTracks)
        });
      }
      | RandomiseAbsolute(id) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: SynthValues.randomValuesAbsolute(state.globalParameters, synthTrack.valueConverter, synthTrack.values)
          }, state.synthTracks)
        });
      }
      | RandomiseRelative(id) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: SynthValues.randomValuesRelative(state.globalParameters, synthTrack.valueConverter, synthTrack.values)
          }, state.synthTracks)
        });
      }
      | Reset(id) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            values: SynthValues.defaultValues(state.globalParameters, synthTrack.valueConverter, SynthValues.valuesLength(synthTrack.values))
          }, state.synthTracks)
        });
      }
      | SetSubTicks(id, subTicks) => {
        ReasonReact.Update({
          ...state,
          synthTracksUndoBuffer: UndoBuffer.write(state.synthTracks, state.synthTracksUndoBuffer),
          synthTracks: SynthTracks.mapSynthTrackById(id, synthTrack => {
            ...synthTrack,
            subTicks
          }, state.synthTracks)
        });
    }
    },

  didMount: (self) => {
    let scheduler = WebAudio.createSchedule((beatTime, beatLength) => {
      self.send(Actions.Playback(beatTime, beatLength));
    });

    scheduler.setBpm(float_of_int(self.state.bpm));

    self.onUnmount(() => scheduler.stop());

    self.state.scheduler := Some(scheduler);
  },

  didUpdate: ({ oldSelf, newSelf }) => {
    if (oldSelf.state.isPlaying !== newSelf.state.isPlaying) {
      switch (newSelf.state.scheduler^) {
        | None => ()
        | Some(scheduler) => {
          if (newSelf.state.isPlaying) {
            newSelf.send(Restart);

            scheduler.start();
          } else {
            scheduler.stop();
          }
        }
      }
    };

    if (oldSelf.state.volume !== newSelf.state.volume) {
      WebAudio.setGlobalVolume(newSelf.state.volume);
    }

    if (oldSelf.state.bpm !== newSelf.state.bpm) {
      switch (newSelf.state.scheduler^) {
        | None => ()
        | Some(scheduler) => scheduler.setBpm(float_of_int(newSelf.state.bpm))
      }
    };
  },

  render: self => {
    <div className="ma4">
      <div className="flex items-center">
        <button
          className="w4 h2 flex-none"
          disabled=(UndoBuffer.isEmpty(self.state.synthTracksUndoBuffer))
          onClick=(_event => self.send(Undo))
        >
          (ReasonReact.string("Undo"))
        </button>
        <button
          className="w4 h2 flex-none"
          disabled=(UndoBuffer.isEmpty(self.state.synthTracksRedoBuffer))
          onClick=(_event => self.send(Redo))
        >
          (ReasonReact.string("Redo"))
        </button>
        <Range
          value=float_of_int(self.state.bpm)
          label=("BPM: " ++ string_of_int(self.state.bpm))
          min=40.0
          max=200.0
          step=1.0
          onChange=(value => self.send(SetBpm(int_of_float(value))))
        />
        <Range
          value=self.state.volume
          label=("Volume: " ++ (Js.Math.floor(self.state.volume *. 100.0) |> string_of_int) ++ "%")
          min=0.0
          max=1.0
          step=0.01
          onChange=(value => self.send(SetVolume(value)))
        />
        <button className="w4 h2 flex-none" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
          (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
        </button>
        <button className="w4 h2 flex-none" onClick=(_event => self.send(Restart))>
          (ReasonReact.string("Restart"))
        </button>
        <button className="w4 h2 flex-none" onClick=(_event => self.send(RandomiseAll(true)))>
          (ReasonReact.string("Randomise All"))
        </button>
        <label className="flex-none">
          <input type_="checkbox" checked=self.state.sync onChange=(event => {
            self.send(SetSync(event->ReactEvent.Form.target##checked));
          }) />
          (ReasonReact.string("Sync"))
        </label>
      </div>
      <div>
        (ReasonReact.array(Array.map(((label, scale)) =>
          <label key=label>
            <input
              type_="radio"
              name="scale"
              value=label
              checked=(scale === self.state.globalParameters.scale)
              onChange=((_event) => self.send(SetScale(scale)))
            />
            (ReasonReact.string(label))
          </label>
        , Scales.scales)))
      </div>
      (
        List.mapi((index, synthTrack:SynthTrack.t) => {
          // TODO - replace with keyed fragment when upgrading reason-react
          <div key=(Id.toString(synthTrack.id))>
            {index > 0 ? <div className="h1 flex-none" /> : ReasonReact.null}
            <Track
              synthTrack
              globalParameters=self.state.globalParameters
              send=self.send
            />
          </div>
        }, self.state.synthTracks)
        |> Array.of_list
        |> ReasonReact.array
      )
    </div>
  },
};
