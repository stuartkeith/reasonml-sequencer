type state = {
  octave: Lane.lane,
  transpose: Lane.lane,
  velocity: Lane.lane,
  pan: Lane.lane,
  chance: Lane.lane,
  offset: Lane.lane,
  length: Lane.lane,
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
  | RandomiseLaneAbsolute(Lane.laneValue);

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

let onSetValue = (send, laneValue, index, value) => send(SetLaneValue(laneValue, index, value));
let onSetLength = (send, laneValue, index) => send(SetLoopAfterIndex(laneValue, index));
let onRandomiseAbsolute = (send, laneValue) => send(RandomiseLaneAbsolute(laneValue));

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

              let note = (octave * 12) + transpose;
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
    },

  didMount: (self) => {
    WebAudio.loadSound("guitar.mp3", (buffer) => {
      self.state.soundBuffer := Some(buffer);
    });

    self.state.scheduler := Some(WebAudio.createSchedule((beatTime, beatLength) => {
      self.send(Playback(beatTime, beatLength));
    }));
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
    let onSetValueBound = onSetValue(self.send);
    let onSetLengthBound = onSetLength(self.send);
    let onRandomiseAbsoluteBound = onRandomiseAbsolute(self.send);

    <div className="ma4">
      <div>
        <button className="w4" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
          (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
        </button>
      </div>
      <div className="h1" />
      <Row
        label="Octave"
        laneValue=Lane.Octave
        lane=self.state.octave
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Transpose"
        laneValue=Lane.Transpose
        lane=self.state.transpose
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Velocity"
        laneValue=Lane.Velocity
        lane=self.state.velocity
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Pan"
        laneValue=Lane.Pan
        lane=self.state.pan
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Chance"
        laneValue=Lane.Chance
        lane=self.state.chance
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Offset"
        laneValue=Lane.Offset
        lane=self.state.offset
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
      <div className="h1" />
      <Row
        label="Length"
        laneValue=Lane.Length
        lane=self.state.length
        onSetValue=onSetValueBound
        onSetLength=onSetLengthBound
        onRandomiseAbsolute=onRandomiseAbsoluteBound
      />
    </div>
  },
};
