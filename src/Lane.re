type t = {
  values: array(int),
  default: int,
  min: int,
  max: int,
  index: int,
  visualIndex: int,
  loopAfterIndex: int
};

let createValues = (default) => Array.make(16, default);

let empty = (default, min, max) => {
  values: createValues(default),
  default,
  min,
  max,
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

let restart = (lane) => {
  ...lane,
  index: 0,
  visualIndex: 0
};

let reset = (lane) => {
  ...lane,
  values: createValues(lane.default)
};

let value = (lane) => lane.values[lane.index];
let loopAfterIndex = (lane) => lane.loopAfterIndex;
let visualIndex = (lane) => lane.visualIndex;
let min = (lane) => lane.min;
let max = (lane) => lane.max;
let values = (lane) => lane.values;

let setLoopAfterIndex = (loopAfterIndex, lane) => {
  ...lane,
  loopAfterIndex
};

let setValue = (index, value, lane) => {
  if (value >= lane.min && value <= lane.max) {
    lane.values[index] = value;
  };

  {
    ...lane,
    loopAfterIndex: Pervasives.max(lane.loopAfterIndex, index)
  };
};

let randomiseAbsolute = (lane) => {
  for (i in 0 to Array.length(lane.values) - 1) {
    lane.values[i] = lane.min + Random.int(lane.max - lane.min + 1);
  };

  lane;
};

let randomiseRelative = (delta, lane) => {
  for (i in 0 to Array.length(lane.values) - 1) {
    let min = Pervasives.max(lane.min, lane.values[i] - delta);
    let max = Pervasives.min(lane.max, lane.values[i] + delta);
    let range = max - min + 1;

    lane.values[i] = min + Random.int(range);
  };

  lane;
};

let setMax = (max, lane) => {
  for (i in 0 to Array.length(lane.values) - 1) {
    lane.values[i] = Pervasives.min(lane.values[i], max);
  };

  {
    ...lane,
    max
  };
};
