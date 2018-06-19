type state = {
  octave: Lane.lane,
  transpose: Lane.lane,
  isPlaying: bool
};

type laneEdit = {
  laneValue:Lane.laneValue,
  index: int,
  value: int
};

type action =
  | Playback
  | AdvancePlayback
  | SetLength(Lane.laneValue, int)
  | SetPlayback(bool)
  | SetLaneValue(laneEdit);

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,

  initialState: () => {
    isPlaying: false,
    octave: Lane.emptyLane(),
    transpose: Lane.emptyLane()
  },

  reducer: (action, state) =>
    switch (action) {
      | Playback => ReasonReact.SideEffects((self) => {
        let octave = self.state.octave.values[self.state.octave.index];
        let transpose = self.state.transpose.values[self.state.transpose.index];
        let note = (octave * 12) + transpose;

        Js.log(note);

        self.send(AdvancePlayback);
      })
      | AdvancePlayback => ReasonReact.Update({
        ...state,
        octave: Lane.advance(state.octave),
        transpose: Lane.advance(state.transpose),
      })
      | SetPlayback(value) => ReasonReact.Update({
        ...state,
        isPlaying: value
      })
      | SetLength(laneValue, index) => switch (laneValue) {
        | Lane.Octave => ReasonReact.Update({
          ...state,
          octave: {
            ...state.octave,
            length: index
          }
        })
        | Lane.Transpose => ReasonReact.Update({
          ...state,
          transpose: {
            ...state.transpose,
            length: index
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
    WebAudio.createSchedule((beatTime, beatLength) => {
      self.send(Playback);
    });
  },

  didUpdate: ({ oldSelf, newSelf }) => {
    if (oldSelf.state.isPlaying !== newSelf.state.isPlaying) {
      Js.log("is has changed - change schedule")
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
        onClick=((index, value) => self.send(SetLaneValue({ laneValue: Lane.Octave, index, value })))
        onSetLength=((index) => self.send(SetLength(Lane.Octave, index)))
      />
      <Row
        label="Transpose"
        lane=self.state.transpose
        onClick=((index, value) => self.send(SetLaneValue({ laneValue: Lane.Transpose, index, value })))
        onSetLength=((index) => self.send(SetLength(Lane.Transpose, index)))
      />
    </div>
  },
};
