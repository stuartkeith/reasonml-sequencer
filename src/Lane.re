type laneValue =
  | Octave
  | Transpose
  | Velocity
  | Pan
  | Chance
  | Offset
  | Length;

type lane = {
  values: array(int),
  index: int,
  visualIndex: int,
  loopAfterIndex: int
};

let getDefaultValue = (laneValue) => switch (laneValue) {
  | Octave => 0
  | Transpose => 0
  | Pan => 0
  | Velocity => 100
  | Chance => 100
  | Offset => 0
  | Length => 100
};

let getMinValue = (laneValue) => switch (laneValue) {
  | Octave => -2
  | Transpose => 0
  | Velocity => 0
  | Pan => -100
  | Chance => 0
  | Offset => 0
  | Length => 0
};

let getMaxValue = (laneValue) => switch (laneValue) {
  | Octave => 2
  | Transpose => 11
  | Velocity => 100
  | Pan => 100
  | Chance => 100
  | Offset => 100
  | Length => 100
};

let emptyLane = (laneValue) => {
  values: Array.make(16, getDefaultValue(laneValue)),
  index: 0,
  visualIndex: 0,
  loopAfterIndex: 0
};

let advance = (lane) => {
  let nextIndex = lane.index + 1;

  {
    ...lane,
    index: nextIndex > lane.loopAfterIndex ? 0 : nextIndex,
    visualIndex: lane.index
  }
};

let reset = (lane) => {
  ...lane,
  index: 0,
  visualIndex: 0
};

let getValue = (lane) => lane.values[lane.index];
