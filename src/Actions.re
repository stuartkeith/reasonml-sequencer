type mouseAction =
  | MouseEnter
  | MouseMove
  | MouseLeave
  | MouseDown
  | MouseUp;

type action =
  | AdvancePlayback
  | RandomiseAll(bool)
  | RandomiseAbsolute(Id.t)
  | RandomiseRelative(Id.t)
  | Redo
  | Reset(Id.t)
  | Restart
  | SetBpm(int)
  | SetLoopAfterIndex(Id.t, int)
  | SetPlayback(bool)
  | SetScale(Scales.t)
  | SetSubTicks(Id.t, int)
  | SetSync(bool)
  | SetVolume(float)
  | TrackEditMode(Id.t, SynthValues.valuesUpdate, mouseAction)
  | Undo;
