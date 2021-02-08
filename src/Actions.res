type action =
  | AdvancePlayback
  | RandomiseAll
  | RandomiseAbsolute(Id.t)
  | RandomiseRelative(Id.t)
  | Redo
  | ResetAll
  | Reset(Id.t)
  | Restart
  | SetBpm(float)
  | SetLoopLength(Id.t, int)
  | SetPlayback(bool)
  | SetSubTicks(Id.t, int)
  | SetSync(bool)
  | SetVolume(float)
  | SetWarble(float)
  | TrackEditMode(Id.t, SynthValues.update, TrackEditMode.mouseAction)
  | Undo
  | UpdateGlobalParameters(SynthParameters.globalParameters);
