type arrayIndex = int;

type laneAction('a) =
  | SetLaneValue(arrayIndex, 'a, bool)
  | SetLoopAfterIndex(arrayIndex)
  | RandomiseLaneAbsolute
  | RandomiseLaneRelative('a)
  | ResetLane
  | SetSubTicks(int);
