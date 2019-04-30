let scales = [|
  ("Chromatic", Scales.Chromatic),
  ("Major", Scales.Major),
  ("Dorian", Scales.Dorian),
  ("Phrygian", Scales.Phrygian),
  ("Lydian", Scales.Lydian),
  ("Mixolydian", Scales.Mixolydian),
  ("Minor", Scales.Minor),
  ("Locrian", Scales.Locrian)
|];

let chords = [|
  ("None", Chords.Solo),
  ("Maj", Chords.Major),
  ("Min", Chords.Minor),
  ("Dom", Chords.Diminished),
  ("Maj7", Chords.MajorSeventh),
  ("Min7", Chords.MinorSeventh),
  ("Dom7", Chords.DominantSeventh),
  ("Sus2", Chords.Suspended2),
  ("Sus4", Chords.Suspened4),
  ("Aug", Chords.Augmented)
|];

type lanes = {
  octave: Lane.t(int, unit),
  transpose: Lane.t(int, unit),
  pitch: Lane.t(int, Scales.t),
  velocity: Lane.t(float, unit),
  pan: Lane.t(float, unit),
  chance: Lane.t(float, unit),
  offset: Lane.t(float, unit),
  length: Lane.t(float, unit),
  filter: Lane.t(float, unit),
  chord: Lane.t(int, array((string, Chords.t)))
};

type state = {
  lanes,
  lanesUndoBuffer: UndoBuffer.t(lanes),
  lanesRedoBuffer: UndoBuffer.t(lanes),
  isPlaying: bool,
  volume: float,
  bpm: int,
  tick: int,
  sync: bool,
  scheduler: ref(option(WebAudio.schedule))
};

type lane(_, _) =
  | Octave: lane(int, unit)
  | Transpose: lane(int, unit)
  | Pitch: lane(int, Scales.t)
  | Velocity: lane(float, unit)
  | Pan: lane(float, unit)
  | Chance: lane(float, unit)
  | Offset: lane(float, unit)
  | Length: lane(float, unit)
  | Filter: lane(float, unit)
  | Chord: lane(int, array((string, Chords.t)));

type action =
  | Playback(float, float)
  | AdvancePlayback
  | RestartLanes
  | SetPlayback(bool)
  | SetSync(bool)
  | RandomiseAll(bool)
  | SetScale(Scales.t)
  | SetVolume(float)
  | SetBpm(int)
  | UpdateLanes(lanes => (lanes, option(lanes)))
  | Undo
  | Redo;

let component = ReasonReact.reducerComponent("App");

// see https://bryangarza.github.io/universal-quantification-in-ocaml.html
type fnWrap = {
  fnWrap: 'a 'b . Lane.t('a, 'b) => Lane.t('a, 'b)
};

let applyToAllLanes = (state, { fnWrap }) => {
  octave: fnWrap(state.octave),
  transpose: fnWrap(state.transpose),
  pitch: fnWrap(state.pitch),
  velocity: fnWrap(state.velocity),
  pan: fnWrap(state.pan),
  chance: fnWrap(state.chance),
  offset: fnWrap(state.offset),
  length: fnWrap(state.length),
  filter: fnWrap(state.filter),
  chord: fnWrap(state.chord)
};

let mergeUndo = (source, target) => {
  octave: Lane.merge(source.octave, target.octave),
  transpose: Lane.merge(source.transpose, target.transpose),
  pitch: Lane.merge(source.pitch, target.pitch),
  velocity: Lane.merge(source.velocity, target.velocity),
  pan: Lane.merge(source.pan, target.pan),
  chance: Lane.merge(source.chance, target.chance),
  offset: Lane.merge(source.offset, target.offset),
  length: Lane.merge(source.length, target.length),
  filter: Lane.merge(source.filter, target.filter),
  chord: Lane.merge(source.chord, target.chord),
};

let updateLane = (type a, type b, lane:lane(a, b), lanes, fn: Lane.t(a, b) => Lane.t(a, b)) => {
  switch (lane) {
    | Octave => { ...lanes, octave: fn(lanes.octave) }
    | Transpose => { ...lanes, transpose: fn(lanes.transpose) }
    | Velocity => { ...lanes, velocity: fn(lanes.velocity) }
    | Pan => { ...lanes, pan: fn(lanes.pan) }
    | Chance => { ...lanes, chance: fn(lanes.chance) }
    | Length => { ...lanes, length: fn(lanes.length) }
    | Filter => { ...lanes, filter: fn(lanes.filter) }
    | Pitch => { ...lanes, pitch: fn(lanes.pitch) }
    | Chord => { ...lanes, chord: fn(lanes.chord) }
    | Offset => { ...lanes, offset: fn(lanes.offset) }
  }
};

let handleLaneAction = (lane, laneAction, lanes) => {
  let partial = updateLane(lane, lanes);

  Actions.(switch (laneAction) {
    | SetLaneValue(array, undoArray) => {
      let undoLanes = switch (undoArray) {
        | Some(undoArrayArray) => Some(partial(lane => Lane.setValues(undoArrayArray, lane)))
        | None => None
      };

      (partial(lane => Lane.setValues(array, lane)), undoLanes);
    }
    | SetLoopAfterIndex(index) => (partial(lane => Lane.setLoopAfterIndex(index, lane)), None)
    | RandomiseLaneAbsolute => (partial(lane => Lane.randomAbsolute(lane) |> Lane.randomLoopAfterIndex), Some(lanes))
    | RandomiseLaneRelative(delta) => (partial(lane => Lane.randomRelative(delta, lane)), Some(lanes))
    | ResetLane => (partial(lane => Lane.reset(lane)), Some(lanes))
    | SetSubTicks(value) => (partial(lane => Lane.setSubTicks(value, lane)), None)
  });
};

let make = (_children) => {
  ...component,

  initialState: () => {
    let scale = switch (Random.int(7)) {
      | 0 => Scales.Major
      | 1 => Scales.Dorian
      | 2 => Scales.Phrygian
      | 3 => Scales.Lydian
      | 4 => Scales.Mixolydian
      | 5 => Scales.Minor
      | 6 => Scales.Locrian
      | _ => raise(Not_found)
    };

    let length = 16;

    let lanes = {
      octave: Lane.create(Parameter.createInt(0, -2, 1), 1, length),
      transpose: Lane.create(Parameter.createInt(0, 0, 11), 16, length),
      pitch: Lane.create(Parameter.createScale(scale), 1, length),
      velocity: Lane.create(Parameter.createFloat(1.0, 0.0, 1.0), 1, length),
      pan: Lane.create(Parameter.createFloat(0.0, -1.0, 1.0), 1, length),
      chance: Lane.create(Parameter.createFloat(1.0, 0.0, 1.0), 1, length),
      offset: Lane.create(Parameter.createFloat(0.0, 0.0, 1.0), 1, length),
      length: Lane.create(Parameter.createFloat(1.0, 0.0, 2.0), 1, length),
      filter: Lane.create(Parameter.createFloat(0.5, 0.0, 1.0), 1, length),
      chord: Lane.create(Parameter.createArray(chords), 1, length)
    };

    {
      lanes,
      lanesUndoBuffer: UndoBuffer.create(12, lanes),
      lanesRedoBuffer: UndoBuffer.create(12, lanes),
      isPlaying: false,
      volume: 1.0,
      bpm: 120,
      tick: 0,
      sync: false,
      scheduler: ref(None)
    }
  },

  reducer: (action, state) =>
    switch (action) {
      | Undo => switch (UndoBuffer.read(state.lanesUndoBuffer)) {
        | None => ReasonReact.NoUpdate
        | Some(lanes) => ReasonReact.Update({
          ...state,
          lanes: mergeUndo(lanes, state.lanes),
          lanesUndoBuffer: UndoBuffer.pop(state.lanesUndoBuffer),
          lanesRedoBuffer: UndoBuffer.write(state.lanes, state.lanesRedoBuffer)
        })
      }
      | Redo => switch (UndoBuffer.read(state.lanesRedoBuffer)) {
        | None => ReasonReact.NoUpdate
        | Some(lanes) => ReasonReact.Update({
          ...state,
          lanes: mergeUndo(lanes, state.lanes),
          lanesUndoBuffer: UndoBuffer.write(state.lanes, state.lanesUndoBuffer),
          lanesRedoBuffer: UndoBuffer.pop(state.lanesRedoBuffer)
        })
      }
      | RestartLanes => ReasonReact.Update({
        ...state,
        lanes: applyToAllLanes(state.lanes, { fnWrap: lane => Lane.restart(lane) }),
        tick: 0
      })
      | Playback(beatTime, beatLength) => ReasonReact.SideEffects((self) => {
        let chance = Random.float(1.);

        let offset = Lane.value(self.state.lanes.offset);

        if (chance < Lane.value(self.state.lanes.chance)) {
          let octave = Lane.value(self.state.lanes.octave);
          let transpose = Lane.value(self.state.lanes.transpose);
          let pitch = Lane.value(self.state.lanes.pitch);
          let velocity = Lane.value(self.state.lanes.velocity);
          let pan = Lane.value(self.state.lanes.pan);
          let length = Lane.value(self.state.lanes.length);
          let filter = Lane.value(self.state.lanes.filter);
          let chordIndex = Lane.value(self.state.lanes.chord);

          let (_, chordType) = chords[chordIndex];
          let chord = Chords.value(chordType);

          let scale = Lane.getParameter(self.state.lanes.pitch).value;
          let pitchScaled = Scales.value(pitch, scale);
          let note = (octave * 12) + transpose + pitchScaled;

          WebAudio.playSynth(~note, ~chord, ~gain=velocity, ~pan, ~start=beatTime +. (beatLength *. offset), ~time=beatLength *. length, ~filter);
        };

        WebAudio.playHihat(~start=beatTime +. (beatLength *. offset));

        self.send(AdvancePlayback);
      })
      | AdvancePlayback => {
        let nextTick = state.tick + 1;
        let sync = state.sync ? Lane.Sync(nextTick) : Lane.NoSync;

        ReasonReact.Update({
          ...state,
          lanes: applyToAllLanes(state.lanes, { fnWrap: lane => Lane.advance(sync, lane) }),
          tick: nextTick
        });
      }
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
      })
      | SetSync(value) => ReasonReact.Update({
        ...state,
        sync: value
      })
      | RandomiseAll(canUndo) => {
        ReasonReact.Update({
          ...state,
          lanesUndoBuffer: canUndo ? UndoBuffer.write(state.lanes, state.lanesUndoBuffer) : state.lanesUndoBuffer,
          lanes: {
            octave: Lane.mapTransform((_, min, max) => min + Random.int(max - min + 1), state.lanes.octave)
              |> Lane.randomLoopAfterIndex,
            transpose: Lane.mapTransform((_, min, max) => min + Random.int(max - min + 1), state.lanes.transpose)
              |> Lane.randomLoopAfterIndex,
            pitch: Lane.mapTransform((_, min, max) => min + Random.int(max - min + 1), state.lanes.pitch)
              |> Lane.randomLoopAfterIndex,
            velocity: Lane.mapTransform((_, _, _) => 0.9 +. Random.float(0.1), state.lanes.velocity)
              |> Lane.randomLoopAfterIndex,
            chance: Lane.mapTransform((_, _, _) => 0.4 +. Random.float(0.6), state.lanes.chance)
              |> Lane.randomLoopAfterIndex,
            pan: Lane.reset(state.lanes.pan),
            offset: Lane.reset(state.lanes.offset),
            length: Lane.mapTransform((_, _, _) => 0.2 +. Random.float(1.4), state.lanes.length)
              |> Lane.randomLoopAfterIndex,
            filter: Lane.mapTransform((_, _, _) => 0.4 +. Random.float(0.6), state.lanes.filter)
              |> Lane.randomLoopAfterIndex,
            chord: state.lanes.chord
          }
        })
      }
      | SetScale(scale) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          pitch: Lane.setParameter(Parameter.createScale(scale), state.lanes.pitch),
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
      | UpdateLanes(updateFn) => {
        let (newLanes, lanesToUndo) = updateFn(state.lanes);

        let lanesUndoBuffer = switch (lanesToUndo) {
          | Some(lanes) => UndoBuffer.write(lanes, state.lanesUndoBuffer)
          | None => state.lanesUndoBuffer
        };

        ReasonReact.Update({
          ...state,
          lanes: newLanes,
          lanesUndoBuffer
        });
      }
    },

  didMount: (self) => {
    self.send(RandomiseAll(false));

    let scheduler = WebAudio.createSchedule((beatTime, beatLength) => {
      self.send(Playback(beatTime, beatLength));
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
            newSelf.send(RestartLanes);

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
    let selectedScale = Lane.getParameter(self.state.lanes.pitch).value;

    let sendLaneAction = (lane, laneAction) => {
      self.send(UpdateLanes(lanes => handleLaneAction(lane, laneAction, lanes)));
    };

    <div className="ma4">
      <div className="flex items-center">
        <button
          className="w4 h2"
          disabled=(UndoBuffer.isEmpty(self.state.lanesUndoBuffer))
          onClick=(_event => self.send(Undo))
        >
          (ReasonReact.string("Undo"))
        </button>
        <button
          className="w4 h2"
          disabled=(UndoBuffer.isEmpty(self.state.lanesRedoBuffer))
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
        <button className="w4 h2" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
          (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
        </button>
        <button className="w4 h2" onClick=(_event => self.send(RestartLanes))>
          (ReasonReact.string("Restart"))
        </button>
        <button className="w4 h2" onClick=(_event => self.send(RandomiseAll(true)))>
          (ReasonReact.string("Randomise All"))
        </button>
        <label>
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
              checked=(scale === selectedScale)
              onChange=((_event) => self.send(SetScale(scale)))
            />
            (ReasonReact.string(label))
          </label>
        , scales)))
      </div>
      <div className="h1" />
      <Row.RowInt
        label="Octave"
        lane=self.state.lanes.octave
        laneType=Octave
        relativeValue=1
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowInt
        label="Transpose"
        lane=self.state.lanes.transpose
        laneType=Transpose
        relativeValue=3
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowInt
        label="Pitch"
        lane=self.state.lanes.pitch
        laneType=Pitch
        relativeValue=3
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Velocity"
        lane=self.state.lanes.velocity
        laneType=Velocity
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Pan"
        lane=self.state.lanes.pan
        laneType=Pan
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Chance"
        lane=self.state.lanes.chance
        laneType=Chance
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Offset"
        lane=self.state.lanes.offset
        laneType=Offset
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Length"
        lane=self.state.lanes.length
        laneType=Length
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowFloat
        label="Filter"
        lane=self.state.lanes.filter
        laneType=Filter
        relativeValue=0.2
        sendLaneAction
      />
      <div className="h1" />
      <Row.RowInt
        label="Chord"
        lane=self.state.lanes.chord
        laneType=Chord
        relativeValue=3
        sendLaneAction
      />
    </div>
  },
};
