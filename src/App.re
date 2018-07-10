[@bs.module] external sound : string = "./assets/electric-piano.mp3";

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

type state = {
  octave: Lane.lane,
  transpose: Lane.lane,
  velocity: Lane.lane,
  pan: Lane.lane,
  chance: Lane.lane,
  offset: Lane.lane,
  length: Lane.lane,
  scale: Scales.t,
  isPlaying: bool,
  scheduler: ref(option(WebAudio.schedule)),
  soundBuffer: ref(option(WebAudio.buffer))
};

type arrayIndex = int;
type arrayValue = int;

type action =
  | Playback(float, float)
  | AdvancePlayback
  | ResetLanes
  | SetLoopAfterIndex(Lane.laneValue, arrayIndex)
  | SetPlayback(bool)
  | SetLaneValue(Lane.laneValue, arrayIndex, arrayValue)
  | RandomiseLaneAbsolute(Lane.laneValue)
  | SetScale(Scales.t);

let component = ReasonReact.reducerComponent("App");

/* apply a function to all lanes */
let applyToAllLanes = (state, fn) => ReasonReact.Update({
  ...state,
  octave: fn(state.octave),
  transpose: fn(state.transpose),
  velocity: fn(state.velocity),
  pan: fn(state.pan),
  chance: fn(state.chance),
  offset: fn(state.offset),
  length: fn(state.length)
});

/* apply a function to one lane */
let applyToLane = (state, laneValue, fn) => ReasonReact.Update(
  switch (laneValue) {
    | Lane.Octave => { ...state, octave: fn(state.octave) }
    | Lane.Transpose => { ...state, transpose: fn(state.transpose) }
    | Lane.Velocity => { ...state, velocity: fn(state.velocity) }
    | Lane.Pan => { ...state, pan: fn(state.pan) }
    | Lane.Chance => { ...state, chance: fn(state.chance) }
    | Lane.Offset => { ...state, offset: fn(state.offset) }
    | Lane.Length => { ...state, length: fn(state.length) }
  }
);

let make = (_children) => {
  ...component,

  initialState: () => {
    isPlaying: false,
    octave: Lane.emptyLane(Lane.Octave),
    transpose: Lane.emptyLane(Lane.Transpose),
    velocity: Lane.emptyLane(Lane.Velocity),
    pan: Lane.emptyLane(Lane.Pan),
    chance: Lane.emptyLane(Lane.Chance),
    offset: Lane.emptyLane(Lane.Offset),
    length: Lane.emptyLane(Lane.Length),
    scale: Scales.Chromatic,
    scheduler: ref(None),
    soundBuffer: ref(None)
  },

  reducer: (action, state) =>
    switch (action) {
      | ResetLanes => applyToAllLanes(state, Lane.reset)
      | Playback(beatTime, _beatLength) => switch(state.soundBuffer^) {
        | None => ReasonReact.NoUpdate
        | Some(buffer) => ReasonReact.SideEffects((self) => {
            let chance = Random.int(101);

            if (chance <= Lane.getValue(self.state.chance)) {
              let octave = Lane.getValue(self.state.octave);
              let transpose = Lane.getValue(self.state.transpose);
              let velocity = Lane.getValue(self.state.velocity);
              let pan = Lane.getValue(self.state.pan);
              let offset = Lane.getValue(self.state.offset);
              let length = Lane.getValue(self.state.length);

              let transposeScaled = Scales.getScaleValue(transpose, self.state.scale);

              let note = (octave * 12) + transposeScaled;
              let gain = float_of_int(velocity) /. 100.;
              let panValue = float_of_int(pan) /. 100.;
              let offsetValue = float_of_int(offset) /. 100.;
              let durationValue = float_of_int(length) /. 100.;

              WebAudio.playBuffer(buffer, note, gain, panValue, beatTime, offsetValue, durationValue);
            };

            self.send(AdvancePlayback);
        })
      }
      | AdvancePlayback => applyToAllLanes(state, Lane.advance)
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
      })
      | SetLoopAfterIndex(laneValue, index) => applyToLane(state, laneValue, (subState) => { ...subState, loopAfterIndex: index })
      | SetLaneValue(laneValue, index, value) => applyToLane(state, laneValue, (subState) => {
        subState.values[index] = value;

        {
          ...subState,
          loopAfterIndex: max(subState.loopAfterIndex, index)
        };
      })
      | RandomiseLaneAbsolute(laneValue) => applyToLane(state, laneValue, (subState) => {
        let minValue = Lane.getMinValue(laneValue);
        let maxValue = Lane.getMaxValue(laneValue);

        for (i in 0 to Array.length(subState.values) - 1) {
          subState.values[i] = minValue + Random.int(maxValue - minValue + 1);
        };

        subState;
      })
      | SetScale(scale) => ReasonReact.Update({
        ...state,
        scale
      })
    },

  didMount: (self) => {
    WebAudio.loadSound(sound, (buffer) => {
      self.state.soundBuffer := Some(buffer);
    });

    let scheduler = WebAudio.createSchedule((beatTime, beatLength) => {
      self.send(Playback(beatTime, beatLength));
    });

    self.state.scheduler := Some(scheduler);

    self.onUnmount(() => scheduler.stop());
  },

  didUpdate: ({ oldSelf, newSelf }) => {
    if (oldSelf.state.isPlaying !== newSelf.state.isPlaying) {
      switch (newSelf.state.scheduler^) {
        | None => ()
        | Some(scheduler) => {
          if (newSelf.state.isPlaying) {
            newSelf.send(ResetLanes);

            scheduler.start();
          } else {
            scheduler.stop();
          }
        }
      }
    }
  },

  render: self => {
    <div className="ma4">
      <div>
        <button className="w4" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
          (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
        </button>
        <div>
          (ReasonReact.array(Array.map(((label, scale)) =>
            <label key=label>
              <input
                _type="radio"
                name="scale"
                value=label
                checked=(scale === self.state.scale)
                onChange=((_event) => self.send(SetScale(scale)))
              />
              (ReasonReact.string(label))
            </label>
          , scales)))
        </div>
      </div>
      <div className="h1" />
      <Row
        label="Octave"
        lane=self.state.octave
        minValue=Lane.getMinValue(Lane.Octave)
        maxValue=Lane.getMaxValue(Lane.Octave)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Octave, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Octave, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Octave)))
      />
      <div className="h1" />
      <Row
        label="Transpose"
        lane=self.state.transpose
        minValue=Lane.getMinValue(Lane.Transpose)
        maxValue=Lane.getMaxValue(Lane.Transpose)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Transpose, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Transpose, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Transpose)))
      />
      <div className="h1" />
      <Row
        label="Velocity"
        lane=self.state.velocity
        minValue=Lane.getMinValue(Lane.Velocity)
        maxValue=Lane.getMaxValue(Lane.Velocity)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Velocity, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Velocity, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Velocity)))
      />
      <div className="h1" />
      <Row
        label="Pan"
        lane=self.state.pan
        minValue=Lane.getMinValue(Lane.Pan)
        maxValue=Lane.getMaxValue(Lane.Pan)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Pan, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Pan, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Pan)))
      />
      <div className="h1" />
      <Row
        label="Chance"
        lane=self.state.chance
        minValue=Lane.getMinValue(Lane.Chance)
        maxValue=Lane.getMaxValue(Lane.Chance)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Chance, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Chance, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Chance)))
      />
      <div className="h1" />
      <Row
        label="Offset"
        lane=self.state.offset
        minValue=Lane.getMinValue(Lane.Offset)
        maxValue=Lane.getMaxValue(Lane.Offset)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Offset, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Offset, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Offset)))
      />
      <div className="h1" />
      <Row
        label="Length"
        lane=self.state.length
        minValue=Lane.getMinValue(Lane.Length)
        maxValue=Lane.getMaxValue(Lane.Length)
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Length, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Length, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Lane.Length)))
      />
    </div>
  },
};
