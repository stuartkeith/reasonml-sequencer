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
  isPlaying: bool,
  volume: float,
  bpm: int,
  tick: int,
  sync: bool,
  scheduler: ref(option(WebAudio.schedule))
};

type arrayIndex = int;

type laneAction('a) =
  | SetLaneValue(arrayIndex, 'a, bool)
  | SetLoopAfterIndex(arrayIndex)
  | RandomiseLaneAbsolute
  | RandomiseLaneRelative('a)
  | ResetLane
  | SetSubTicks(int);

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
  | RandomiseAll
  | SetScale(Scales.t)
  | SetVolume(float)
  | SetBpm(int)
  | UpdateLanes(lanes);

let component = ReasonReact.reducerComponent("App");

/* see https://bryangarza.github.io/universal-quantification-in-ocaml.html */
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

let handleLaneAction = (lane, laneAction, state) => {
  let partial = updateLane(lane, state.lanes);

  switch (laneAction) {
    | SetLaneValue(index, value, setLength) => partial(lane => Lane.setValue(index, value, setLength, lane))
    | SetLoopAfterIndex(index) => partial(lane => Lane.setLoopAfterIndex(index, lane))
    | RandomiseLaneAbsolute => partial(lane => Lane.randomAbsolute(lane) |> Lane.randomLoopAfterIndex)
    | RandomiseLaneRelative(delta) => partial(lane => Lane.randomRelative(delta, lane))
    | ResetLane => partial(lane => Lane.reset(lane))
    | SetSubTicks(value) => partial(lane => Lane.setSubTicks(value, lane))
  };
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

    {
      lanes: {
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
      },
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
      | RandomiseAll => ReasonReact.Update({
        ...state,
        lanes: {
          octave: Lane.map((_, min, max) => min + Random.int(max - min + 1), state.lanes.octave)
            |> Lane.randomLoopAfterIndex,
          transpose: Lane.map((_, min, max) => min + Random.int(max - min + 1), state.lanes.transpose)
            |> Lane.randomLoopAfterIndex,
          pitch: Lane.map((_, min, max) => min + Random.int(max - min + 1), state.lanes.pitch)
            |> Lane.randomLoopAfterIndex,
          velocity: Lane.map((_, _, _) => 0.9 +. Random.float(0.1), state.lanes.velocity)
            |> Lane.randomLoopAfterIndex,
          chance: Lane.map((_, _, _) => 0.4 +. Random.float(0.6), state.lanes.chance)
            |> Lane.randomLoopAfterIndex,
          pan: Lane.reset(state.lanes.pan),
          offset: Lane.reset(state.lanes.offset),
          length: Lane.map((_, _, _) => 0.2 +. Random.float(1.4), state.lanes.length)
            |> Lane.randomLoopAfterIndex,
          filter: Lane.map((_, _, _) => 0.4 +. Random.float(0.6), state.lanes.filter)
            |> Lane.randomLoopAfterIndex,
          chord: state.lanes.chord
        }
      })
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
      | UpdateLanes(lanes) => ReasonReact.Update({
        ...state,
        lanes
      })
    },

  didMount: (self) => {
    self.send(RandomiseAll);

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

    let sendLaneAction = (lane, laneAction) => self.send(UpdateLanes(handleLaneAction(lane, laneAction, self.state)));

    <div className="ma4">
      <div className="flex items-center">
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
          label=("Volume: " ++ Utils.round(self.state.volume *. 100.0) ++ "%")
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
        <button className="w4 h2" onClick=(_event => self.send(RandomiseAll))>
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
        onSetSubTicks=((value) => sendLaneAction(Octave, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Octave, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Octave, RandomiseLaneRelative(1)))
        onResetLane=(() => sendLaneAction(Octave, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Octave, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Octave, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowInt
        label="Transpose"
        lane=self.state.lanes.transpose
        onSetSubTicks=((value) => sendLaneAction(Transpose, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Transpose, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Transpose, RandomiseLaneRelative(3)))
        onResetLane=(() => sendLaneAction(Transpose, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Transpose, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Transpose, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowInt
        label="Pitch"
        lane=self.state.lanes.pitch
        onSetSubTicks=((value) => sendLaneAction(Pitch, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Pitch, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Pitch, RandomiseLaneRelative(3)))
        onResetLane=(() => sendLaneAction(Pitch, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Pitch, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Pitch, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Velocity"
        lane=self.state.lanes.velocity
        onSetSubTicks=((value) => sendLaneAction(Velocity, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Velocity, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Velocity, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Velocity, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Velocity, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Velocity, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Pan"
        lane=self.state.lanes.pan
        onSetSubTicks=((value) => sendLaneAction(Pan, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Pan, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Pan, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Pan, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Pan, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Pan, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Chance"
        lane=self.state.lanes.chance
        onSetSubTicks=((value) => sendLaneAction(Chance, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Chance, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Chance, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Chance, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Chance, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Chance, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Offset"
        lane=self.state.lanes.offset
        onSetSubTicks=((value) => sendLaneAction(Offset, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Offset, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Offset, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Offset, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Offset, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Offset, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Length"
        lane=self.state.lanes.length
        onSetSubTicks=((value) => sendLaneAction(Length, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Length, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Length, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Length, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Length, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Length, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Filter"
        lane=self.state.lanes.filter
        onSetSubTicks=((value) => sendLaneAction(Filter, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Filter, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Filter, RandomiseLaneRelative(0.2)))
        onResetLane=(() => sendLaneAction(Filter, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Filter, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Filter, SetLoopAfterIndex(index)))
      />
      <div className="h1" />
      <Row.RowInt
        label="Chord"
        lane=self.state.lanes.chord
        onSetSubTicks=((value) => sendLaneAction(Chord, SetSubTicks(value)))
        onRandomiseAbsolute=(() => sendLaneAction(Chord, RandomiseLaneAbsolute))
        onRandomiseRelative=(() => sendLaneAction(Chord, RandomiseLaneRelative(3)))
        onResetLane=(() => sendLaneAction(Chord, ResetLane))
        onSetValue=((index, value, setLength) => sendLaneAction(Chord, SetLaneValue(index, value, setLength)))
        onSetLength=((index) => sendLaneAction(Chord, SetLoopAfterIndex(index)))
      />
    </div>
  },
};
