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

let randomLoopAfterIndex = (lane) => {
  ...lane,
  loopAfterIndex: Random.int(Array.length(lane.values))
};

let setValue = (index, value, setLength, lane) => {
  if (value >= lane.min && value <= lane.max) {
    lane.values[index] = value;
  };

  {
    ...lane,
    loopAfterIndex: setLength ? Pervasives.max(lane.loopAfterIndex, index) : lane.loopAfterIndex
  };
};

let map = (fn, lane) => {
  {
    ...lane,
    values: Array.map(value => {
      let value = fn(value, lane.min, lane.max);

      if (value < lane.min) {
        lane.min;
      } else if (value > lane.max) {
        lane.max;
      } else {
        value;
      };
    }, lane.values)
  };
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
