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

type lanes = {
  octave: Lane.t(int, unit),
  transpose: Lane.t(int, unit),
  pitch: Lane.t(int, Scales.t),
  velocity: Lane.t(float, unit),
  pan: Lane.t(float, unit),
  chance: Lane.t(float, unit),
  offset: Lane.t(float, unit),
  length: Lane.t(float, unit),
  filter: Lane.t(float, unit)
};

type state = {
  lanes,
  isPlaying: bool,
  bpm: int,
  scheduler: ref(option(WebAudio.schedule))
};

type arrayIndex = int;

type laneAction('a) =
  | SetLaneValue(arrayIndex, 'a, bool)
  | SetLoopAfterIndex(arrayIndex)
  | RandomiseLaneAbsolute
  | RandomiseLaneRelative('a)
  | ResetLane;

type action =
  | Playback(float, float)
  | AdvancePlayback
  | RestartLanes
  | SetPlayback(bool)
  | RandomiseAll
  | SetScale(Scales.t)
  | SetBpm(int)
  | Octave(laneAction(int))
  | Transpose(laneAction(int))
  | Velocity(laneAction(float))
  | Pan(laneAction(float))
  | Chance(laneAction(float))
  | Offset(laneAction(float))
  | Length(laneAction(float))
  | Filter(laneAction(float))
  | Pitch(laneAction(int));

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
  filter: fnWrap(state.filter)
};

let handleLaneAction = (laneAction, lane) => {
  switch (laneAction) {
    | SetLaneValue(index, value, setLength) => Lane.setValue(index, value, setLength, lane)
    | SetLoopAfterIndex(index) => Lane.setLoopAfterIndex(index, lane)
    | RandomiseLaneAbsolute => Lane.randomAbsolute(lane) |> Lane.randomLoopAfterIndex
    | RandomiseLaneRelative(delta) => Lane.randomRelative(delta, lane)
    | ResetLane => Lane.reset(lane)
  };
};

let bpmParameter = Parameter.createInt(120, 40, 200);

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
        transpose: Lane.create(Parameter.createInt(0, 0, 11), 64, length),
        pitch: Lane.create(Parameter.createScale(scale), 1, length),
        velocity: Lane.create(Parameter.createFloat(1.0, 0.0, 1.0), 1, length),
        pan: Lane.create(Parameter.createFloat(0.0, -1.0, 1.0), 1, length),
        chance: Lane.create(Parameter.createFloat(1.0, 0.0, 1.0), 1, length),
        offset: Lane.create(Parameter.createFloat(0.0, 0.0, 1.0), 1, length),
        length: Lane.create(Parameter.createFloat(1.0, 0.0, 2.0), 1, length),
        filter: Lane.create(Parameter.createFloat(0.5, 0.0, 1.0), 1, length)
      },
      isPlaying: false,
      bpm: 120,
      scheduler: ref(None)
    }
  },

  reducer: (action, state) =>
    switch (action) {
      | RestartLanes => ReasonReact.Update({
        ...state,
        lanes: applyToAllLanes(state.lanes, { fnWrap: lane => Lane.restart(lane) })
      })
      | Playback(beatTime, beatLength) => ReasonReact.SideEffects((self) => {
        let chance = Random.float(1.);

        if (chance < Lane.value(self.state.lanes.chance)) {
          let octave = Lane.value(self.state.lanes.octave);
          let transpose = Lane.value(self.state.lanes.transpose);
          let pitch = Lane.value(self.state.lanes.pitch);
          let velocity = Lane.value(self.state.lanes.velocity);
          let pan = Lane.value(self.state.lanes.pan);
          let offset = Lane.value(self.state.lanes.offset);
          let length = Lane.value(self.state.lanes.length);
          let filter = Lane.value(self.state.lanes.filter);

          let scale = Lane.getParameter(self.state.lanes.pitch).value;
          let pitchScaled = Scales.value(pitch, scale);
          let note = (octave * 12) + transpose + pitchScaled;

          WebAudio.playSynth(~note, ~gain=velocity, ~pan, ~start=beatTime +. (beatLength *. offset), ~time=beatLength *. length, ~filter);
        };

        WebAudio.playHihat(~start=beatTime);

        self.send(AdvancePlayback);
      })
      | AdvancePlayback => ReasonReact.Update({
        ...state,
        lanes: applyToAllLanes(state.lanes, { fnWrap: lane => Lane.advance(lane) })
      })
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
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
            |> Lane.randomLoopAfterIndex
        }
      })
      | SetScale(scale) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          pitch: Lane.setParameter(Parameter.createScale(scale), state.lanes.pitch),
        }
      })
      | SetBpm(bpm) => ReasonReact.Update({
        ...state,
        bpm
      })
      | Octave(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          octave: handleLaneAction(laneAction, state.lanes.octave)
        }
      })
      | Transpose(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          transpose: handleLaneAction(laneAction, state.lanes.transpose)
        }
      })
      | Velocity(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          velocity: handleLaneAction(laneAction, state.lanes.velocity)
        }
      })
      | Pan(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          pan: handleLaneAction(laneAction, state.lanes.pan)
        }
      })
      | Chance(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          chance: handleLaneAction(laneAction, state.lanes.chance)
        }
      })
      | Offset(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          offset: handleLaneAction(laneAction, state.lanes.offset)
        }
      })
      | Length(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          length: handleLaneAction(laneAction, state.lanes.length)
        }
      })
      | Filter(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          filter: handleLaneAction(laneAction, state.lanes.filter)
        }
      })
      | Pitch(laneAction) => ReasonReact.Update({
        ...state,
        lanes: {
          ...state.lanes,
          pitch: handleLaneAction(laneAction, state.lanes.pitch)
        }
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

    if (oldSelf.state.bpm !== newSelf.state.bpm) {
      switch (newSelf.state.scheduler^) {
        | None => ()
        | Some(scheduler) => scheduler.setBpm(float_of_int(newSelf.state.bpm))
      }
    };
  },

  render: self => {
    let selectedScale = Lane.getParameter(self.state.lanes.pitch).value;

    <div className="ma4">
      <div>
        <button className="w4" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
          (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
        </button>
        <button className="w4" onClick=(_event => self.send(RestartLanes))>
          (ReasonReact.string("Restart"))
        </button>
        <Slider.SliderInt
          cells=[|self.state.bpm|]
          toFloat=bpmParameter.toFloat
          fromFloat=bpmParameter.fromFloat
          getLabel=bpmParameter.toString
          highlightedIndex=0
          disabledAfterIndex=1
          onSetValue=((_, value, _) => self.send(SetBpm(value)))
          onSetLength=((_length) => ())
        />
        <button className="w4" onClick=(_event => self.send(RandomiseAll))>
          (ReasonReact.string("Randomise All"))
        </button>
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
        onRandomiseAbsolute=(() => self.send(Octave(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Octave(RandomiseLaneRelative(1))))
        onResetLane=(() => self.send(Octave(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Octave(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Octave(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowInt
        label="Transpose"
        lane=self.state.lanes.transpose
        onRandomiseAbsolute=(() => self.send(Transpose(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Transpose(RandomiseLaneRelative(3))))
        onResetLane=(() => self.send(Transpose(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Transpose(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Transpose(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowInt
        label="Pitch"
        lane=self.state.lanes.pitch
        onRandomiseAbsolute=(() => self.send(Pitch(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Pitch(RandomiseLaneRelative(3))))
        onResetLane=(() => self.send(Pitch(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Pitch(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Pitch(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Velocity"
        lane=self.state.lanes.velocity
        onRandomiseAbsolute=(() => self.send(Velocity(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Velocity(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Velocity(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Velocity(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Velocity(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Pan"
        lane=self.state.lanes.pan
        onRandomiseAbsolute=(() => self.send(Pan(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Pan(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Pan(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Pan(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Pan(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Chance"
        lane=self.state.lanes.chance
        onRandomiseAbsolute=(() => self.send(Chance(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Chance(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Chance(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Chance(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Chance(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Offset"
        lane=self.state.lanes.offset
        onRandomiseAbsolute=(() => self.send(Offset(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Offset(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Offset(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Offset(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Offset(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Length"
        lane=self.state.lanes.length
        onRandomiseAbsolute=(() => self.send(Length(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Length(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Length(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Length(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Length(SetLoopAfterIndex(index))))
      />
      <div className="h1" />
      <Row.RowFloat
        label="Filter"
        lane=self.state.lanes.filter
        onRandomiseAbsolute=(() => self.send(Filter(RandomiseLaneAbsolute)))
        onRandomiseRelative=(() => self.send(Filter(RandomiseLaneRelative(0.2))))
        onResetLane=(() => self.send(Filter(ResetLane)))
        onSetValue=((index, value, setLength) => self.send(Filter(SetLaneValue(index, value, setLength))))
        onSetLength=((index) => self.send(Filter(SetLoopAfterIndex(index))))
      />
    </div>
  },
};
