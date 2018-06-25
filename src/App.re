type state = {
  octave: Lane.lane,
  transpose: Lane.lane,
  velocity: Lane.lane,
  isPlaying: bool,
  scheduler: ref(option(WebAudio.schedule)),
  soundBuffer: ref(option(WebAudio.buffer))
};

type arrayIndex = int;
type arrayValue = int;

let maxVelocity = 100;

type action =
  | Playback(float, float)
  | AdvancePlayback
  | ResetLanes
  | SetLoopAfterIndex(Lane.laneValue, arrayIndex)
  | SetPlayback(bool)
  | SetLaneValue(Lane.laneValue, arrayIndex, arrayValue);

let component = ReasonReact.reducerComponent("App");

/* apply a function to all lanes */
let applyToAllLanes = (state, fn) => ReasonReact.Update({
  ...state,
  octave: fn(state.octave),
  transpose: fn(state.transpose),
  velocity: fn(state.velocity)
});

/* apply a function to one lane */
let applyToLane = (state, laneValue, fn) => ReasonReact.Update(
  switch (laneValue) {
    | Lane.Octave => { ...state, octave: fn(state.octave) }
    | Lane.Transpose => { ...state, transpose: fn(state.transpose) }
    | Lane.Velocity => { ...state, velocity: fn(state.velocity) }
  }
);

let make = (_children) => {
  ...component,

  initialState: () => {
    isPlaying: false,
    octave: Lane.emptyLane(0),
    transpose: Lane.emptyLane(0),
    velocity: Lane.emptyLane(maxVelocity),
    scheduler: ref(None),
    soundBuffer: ref(None)
  },

  reducer: (action, state) =>
    switch (action) {
      | ResetLanes => applyToAllLanes(state, Lane.reset)
      | Playback(beatTime, _beatLength) => switch(state.soundBuffer^) {
        | None => ReasonReact.NoUpdate
        | Some(buffer) => ReasonReact.SideEffects((self) => {
            let octave = self.state.octave.values[self.state.octave.index];
            let transpose = self.state.transpose.values[self.state.transpose.index];
            let velocity = self.state.velocity.values[self.state.velocity.index];
            let note = (octave * 12) + transpose;

            let gain = float_of_int(velocity) /. float_of_int(maxVelocity);

            WebAudio.playBuffer(buffer, note, gain, beatTime, 0., 1.);

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

        subState;
      })
    },

  didMount: (self) => {
    WebAudio.loadSound("harp.mp3", (buffer) => {
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
    <div className="ma4">
      <button className="w4" onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
        (self.state.isPlaying ? ReasonReact.string("Stop") : ReasonReact.string("Play"))
      </button>
      <Row
        label="Octave"
        lane=self.state.octave
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Octave, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Octave, index)))
      />
      <Row
        label="Transpose"
        lane=self.state.transpose
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Transpose, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Transpose, index)))
      />
      <Row
        label="Velocity"
        lane=self.state.velocity
        onSetValue=((index, value) => self.send(SetLaneValue(Lane.Velocity, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Velocity, index)))
      />
    </div>
  },
};
