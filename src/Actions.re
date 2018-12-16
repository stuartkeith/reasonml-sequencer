type arrayIndex = int;

type laneAction('a) =
  | SetLaneValue(Lane.values('a), option(Lane.values('a)))
  | SetLoopAfterIndex(arrayIndex)
  | RandomiseLaneAbsolute
  | RandomiseLaneRelative('a)
  | ResetLane
  | SetSubTicks(int);
