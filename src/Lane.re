type laneValue =
  | Octave
  | Transpose;

type lane = {
  values: array(int),
  index: int,
  visualIndex: int,
  loopAfterIndex: int
};

let emptyLane = () => {
  values: Array.make(16, 0),
  index: 0,
  visualIndex: 0,
  loopAfterIndex: 7
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
