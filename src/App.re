[@bs.module] external sound : string = "./assets/electric-piano.mp3";
[@bs.module] external hihat : string = "./assets/hihat.mp3";

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

let keys = [|
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "11"
|];

type laneValue =
  | Octave
  | Transpose
  | Velocity
  | Pan
  | Chance
  | Offset
  | Length;

type lanes = {
  octave: Lane.t,
  transpose: Lane.t,
  velocity: Lane.t,
  pan: Lane.t,
  chance: Lane.t,
  offset: Lane.t,
  length: Lane.t
};

type state = {
  lanes,
  scale: Scales.t,
  globalTranspose: int,
  isPlaying: bool,
  scheduler: ref(option(WebAudio.schedule)),
  soundBuffer: ref(option(WebAudio.buffer)),
  hihatBuffer: ref(option(WebAudio.buffer))
};

type arrayIndex = int;
type arrayValue = int;

type action =
  | Playback(float, float)
  | AdvancePlayback
  | RestartLanes
  | SetLoopAfterIndex(laneValue, arrayIndex)
  | SetPlayback(bool)
  | SetLaneValue(laneValue, arrayIndex, arrayValue)
  | RandomiseLaneAbsolute(laneValue)
  | RandomiseLaneRelative(laneValue, int)
  | ResetLane(laneValue)
  | SetScale(Scales.t)
  | SetGlobalTranspose(int);

let component = ReasonReact.reducerComponent("App");

/* apply a function to all lanes */
let applyToAllLanes = (state, fn) => {
  octave: fn(state.octave),
  transpose: fn(state.transpose),
  velocity: fn(state.velocity),
  pan: fn(state.pan),
  chance: fn(state.chance),
  offset: fn(state.offset),
  length: fn(state.length)
};

/* apply a function to one lane */
let applyToLane = (state, laneValue, fn) => switch (laneValue) {
  | Octave => { ...state, octave: fn(state.octave) }
  | Transpose => { ...state, transpose: fn(state.transpose) }
  | Velocity => { ...state, velocity: fn(state.velocity) }
  | Pan => { ...state, pan: fn(state.pan) }
  | Chance => { ...state, chance: fn(state.chance) }
  | Offset => { ...state, offset: fn(state.offset) }
  | Length => { ...state, length: fn(state.length) }
};

let make = (_children) => {
  ...component,

  initialState: () => {
    let scale = Scales.Chromatic;

    {
      lanes: {
        octave: Lane.empty(0, -2, 2),
        transpose: Lane.empty(0, 0, Scales.max(scale)),
        velocity: Lane.empty(100, 0, 100),
        pan: Lane.empty(0, -100, 100),
        chance: Lane.empty(100, 0, 100),
        offset: Lane.empty(0, 0, 100),
        length: Lane.empty(100, 0, 100)
      },
      isPlaying: false,
      scale,
      globalTranspose: 0,
      scheduler: ref(None),
      soundBuffer: ref(None),
      hihatBuffer: ref(None)
    }
  },

  reducer: (action, state) =>
    switch (action) {
      | RestartLanes => ReasonReact.Update({
        ...state,
        lanes: applyToAllLanes(state.lanes, Lane.restart)
      })
      | Playback(beatTime, _beatLength) => ReasonReact.SideEffects((self) => {
        let chance = Random.int(101);

        if (chance < Lane.value(self.state.lanes.chance)) {
          let octave = Lane.value(self.state.lanes.octave);
          let transpose = Lane.value(self.state.lanes.transpose);
          let velocity = Lane.value(self.state.lanes.velocity);
          let pan = Lane.value(self.state.lanes.pan);
          let offset = Lane.value(self.state.lanes.offset);
          let length = Lane.value(self.state.lanes.length);

          let transposeScaled = Scales.value(transpose, self.state.scale);

          let note = self.state.globalTranspose + (octave * 12) + transposeScaled;
          let gain = float_of_int(velocity) /. 100.;
          let panValue = float_of_int(pan) /. 100.;
          let offsetValue = float_of_int(offset) /. 100.;
          let durationValue = float_of_int(length) /. 100.;

          switch(state.soundBuffer^) {
            | None => ()
            | Some(buffer) => WebAudio.playBuffer(buffer, note, gain, panValue, beatTime, offsetValue, durationValue)
          };
        };

        switch(state.hihatBuffer^) {
          | None => ()
          | Some(buffer) => WebAudio.playBuffer(buffer, 0, 1. -. Random.float(0.5), 0., beatTime, 0., 1.);
        };

        self.send(AdvancePlayback);
      })
      | AdvancePlayback => ReasonReact.Update({
        ...state,
        lanes: applyToAllLanes(state.lanes, Lane.advance)
      })
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
      })
      | SetLoopAfterIndex(laneValue, index) => ReasonReact.Update({
        ...state,
        lanes: applyToLane(state.lanes, laneValue, Lane.setLoopAfterIndex(index))
      })
      | SetLaneValue(laneValue, index, value) => ReasonReact.Update({
        ...state,
        lanes: applyToLane(state.lanes, laneValue, Lane.setValue(index, value))
      })
      | RandomiseLaneAbsolute(laneValue) => ReasonReact.Update({
        ...state,
        lanes: applyToLane(state.lanes, laneValue, Lane.randomiseAbsolute)
      })
      | ResetLane(laneValue) => ReasonReact.Update({
        ...state,
        lanes: applyToLane(state.lanes, laneValue, Lane.reset)
      })
      | RandomiseLaneRelative(laneValue, delta) => ReasonReact.Update({
        ...state,
        lanes: applyToLane(state.lanes, laneValue, Lane.randomiseRelative(delta))
      })
      | SetScale(scale) => ReasonReact.Update({
        ...state,
        scale,
        lanes: {
          ...state.lanes,
          transpose: Lane.setMax(Scales.max(scale), state.lanes.transpose),
        }
      })
      | SetGlobalTranspose(globalTranspose) => ReasonReact.Update({
        ...state,
        globalTranspose
      })
    },

  didMount: (self) => {
    WebAudio.loadSound(sound, (buffer) => {
      self.state.soundBuffer := Some(buffer);
    });

    WebAudio.loadSound(hihat, (buffer) => {
      self.state.hihatBuffer := Some(buffer);
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
            newSelf.send(RestartLanes);

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
          (ReasonReact.array(Array.mapi((i, key) =>
            <label key=key>
              <input
                _type="radio"
                name="key"
                value=key
                checked=(i === self.state.globalTranspose)
                onChange=((_event) => self.send(SetGlobalTranspose(i)))
              />
              (ReasonReact.string(key))
            </label>
          , keys)))
        </div>
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
        lane=self.state.lanes.octave
        onSetValue=((index, value) => self.send(SetLaneValue(Octave, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Octave, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Octave)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Octave, 1)))
        onResetLane=(() => self.send(ResetLane(Octave)))
      />
      <div className="h1" />
      <Row
        label="Transpose"
        lane=self.state.lanes.transpose
        onSetValue=((index, value) => self.send(SetLaneValue(Transpose, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Transpose, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Transpose)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Transpose, 3)))
        onResetLane=(() => self.send(ResetLane(Transpose)))
      />
      <div className="h1" />
      <Row
        label="Velocity"
        lane=self.state.lanes.velocity
        onSetValue=((index, value) => self.send(SetLaneValue(Velocity, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Velocity, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Velocity)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Velocity, 20)))
        onResetLane=(() => self.send(ResetLane(Velocity)))
      />
      <div className="h1" />
      <Row
        label="Pan"
        lane=self.state.lanes.pan
        onSetValue=((index, value) => self.send(SetLaneValue(Pan, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Pan, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Pan)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Pan, 20)))
        onResetLane=(() => self.send(ResetLane(Pan)))
      />
      <div className="h1" />
      <Row
        label="Chance"
        lane=self.state.lanes.chance
        onSetValue=((index, value) => self.send(SetLaneValue(Chance, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Chance, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Chance)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Chance, 10)))
        onResetLane=(() => self.send(ResetLane(Chance)))
      />
      <div className="h1" />
      <Row
        label="Offset"
        lane=self.state.lanes.offset
        onSetValue=((index, value) => self.send(SetLaneValue(Offset, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Offset, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Offset)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Offset, 10)))
        onResetLane=(() => self.send(ResetLane(Offset)))
      />
      <div className="h1" />
      <Row
        label="Length"
        lane=self.state.lanes.length
        onSetValue=((index, value) => self.send(SetLaneValue(Length, index, value)))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Length, index)))
        onRandomiseAbsolute=(() => self.send(RandomiseLaneAbsolute(Length)))
        onRandomiseRelative=(() => self.send(RandomiseLaneRelative(Length, 10)))
        onResetLane=(() => self.send(ResetLane(Length)))
      />
    </div>
  },
};
