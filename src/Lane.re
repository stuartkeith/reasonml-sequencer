type laneValue =
  | Octave
  | Transpose;

type lane = {
  values: array(int),
  index: int,
  length: int
};

let emptyLane = () => {
  values: Array.make(16, 0),
  index: 0,
  length: 8
};

let advance = (lane) => {
  {
    ...lane,
    index: lane.index + 1 >= lane.length ? 0 : lane.index + 1
  }
};
