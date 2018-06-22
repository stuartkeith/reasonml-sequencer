type state = {
  octave: Lane.lane,
  transpose: Lane.lane,
  isPlaying: bool,
  scheduler: ref(option(WebAudio.schedule)),
  soundBuffer: ref(option(WebAudio.buffer))
};

type laneEdit = {
  laneValue:Lane.laneValue,
  index: int,
  value: int
};

type action =
  | Playback(float, float)
  | AdvancePlayback
  | ResetLanes
  | SetLoopAfterIndex(Lane.laneValue, int)
  | SetPlayback(bool)
  | SetLaneValue(laneEdit);

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,

  initialState: () => {
    isPlaying: false,
    octave: Lane.emptyLane(),
    transpose: Lane.emptyLane(),
    scheduler: ref(None),
    soundBuffer: ref(None)
  },

  reducer: (action, state) =>
    switch (action) {
      | ResetLanes => ReasonReact.Update({
        ...state,
        octave: {
          ...state.octave,
          index: 0,
          visualIndex: 0
        },
        transpose: {
          ...state.transpose,
          index: 0,
          visualIndex: 0
        }
      })
      | Playback(beatTime, _beatLength) => switch(state.soundBuffer^) {
        | None => ReasonReact.NoUpdate
        | Some(buffer) => ReasonReact.SideEffects((self) => {
            let octave = self.state.octave.values[self.state.octave.index];
            let transpose = self.state.transpose.values[self.state.transpose.index];
            let note = (octave * 12) + transpose;

            WebAudio.playBuffer(buffer, note, 1., beatTime, 0., 1.);

            self.send(AdvancePlayback);
        })
      }
      | AdvancePlayback => ReasonReact.Update({
        ...state,
        octave: Lane.advance(state.octave),
        transpose: Lane.advance(state.transpose),
      })
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
      })
      | SetLoopAfterIndex(laneValue, index) => switch (laneValue) {
        | Lane.Octave => ReasonReact.Update({
          ...state,
          octave: {
            ...state.octave,
            loopAfterIndex: index
          }
        })
        | Lane.Transpose => ReasonReact.Update({
          ...state,
          transpose: {
            ...state.transpose,
            loopAfterIndex: index
          }
        })
      }
      | SetLaneValue(laneEdit) => {
        switch (laneEdit.laneValue) {
        | Lane.Octave => state.octave.values[laneEdit.index] = laneEdit.value
        | Lane.Transpose => state.transpose.values[laneEdit.index] = laneEdit.value
        };

        ReasonReact.Update(state);
      }
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
    <div>
      <button onClick=(_event => self.send(SetPlayback(!self.state.isPlaying)))>
        (self.state.isPlaying ? ReasonReact.string("Playing") : ReasonReact.string("Stopped"))
      </button>
      <Row
        label="Octave"
        lane=self.state.octave
        onSetValue=((index, value) => self.send(SetLaneValue({ laneValue: Lane.Octave, index, value })))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Octave, index)))
      />
      <Row
        label="Transpose"
        lane=self.state.transpose
        onSetValue=((index, value) => self.send(SetLaneValue({ laneValue: Lane.Transpose, index, value })))
        onSetLength=((index) => self.send(SetLoopAfterIndex(Lane.Transpose, index)))
      />
    </div>
  },
};
