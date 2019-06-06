type action =
  | AdvancePlayback
  | Playback(float, float)
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
  | SetValues(Id.t, SynthValues.values, option(SynthValues.values))
  | SetVolume(float)
  | Undo;
