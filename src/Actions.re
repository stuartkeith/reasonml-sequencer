type mouseAction =
  | MouseEnter
  | MouseMove
  | MouseLeave
  | MouseDown
  | MouseUp;

type action =
  | AdvancePlayback(WebAudio.scheduleTime)
  | RandomiseAll
  | RandomiseAbsolute(Id.t)
  | RandomiseRelative(Id.t)
  | Redo
  | Reset(Id.t)
  | Restart
  | SetBpm(float)
  | SetLoopLength(Id.t, int)
  | SetPlayback(bool)
  | SetScale(Scales.t)
  | SetSubTicks(Id.t, int)
  | SetSync(bool)
  | SetVolume(float)
  | SetWarble(float)
  | TrackEditMode(Id.t, SynthValues.valuesUpdate, mouseAction)
  | Undo;
