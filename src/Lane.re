type t('a, 'b) = {
  parameter: Parameter.t('a, 'b),
  values: array('a),
  subTicks: int,
  index: int,
  subIndex: int,
  visualIndex: int,
  loopAfterIndex: int
};

let create = (parameter, subTicks, length) => {
  parameter,
  values: Array.make(length, parameter.default),
  subTicks,
  index: 0,
  subIndex: 0,
  visualIndex: 0,
  loopAfterIndex: length > 2 ? (length / 2) - 1 : 0
};

let advance = (lane) => {
  let nextSubIndex = lane.subIndex + 1;

  let (subIndex, index, visualIndex) = if (nextSubIndex >= lane.subTicks) {
    let nextIndex = lane.index >= lane.loopAfterIndex ? 0 : lane.index + 1;

    (0, nextIndex, lane.index);
  } else {
    (nextSubIndex, lane.index, lane.visualIndex);
  };

  {
    ...lane,
    index,
    subIndex,
    visualIndex
  }
};

let restart = (lane) => {
  ...lane,
  index: 0,
  subIndex: 0,
  visualIndex: 0
};

let reset = (lane) => {
  ...lane,
  values: Array.map(_ => lane.parameter.default, lane.values)
};

let value = (lane) => lane.values[lane.index];
let loopAfterIndex = (lane) => lane.loopAfterIndex;
let visualIndex = (lane) => lane.visualIndex;
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
  if (value >= lane.parameter.min && value <= lane.parameter.max) {
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
      let value = fn(value, lane.parameter.min, lane.parameter.max);

      if (value < lane.parameter.min) {
        lane.parameter.min;
      } else if (value > lane.parameter.max) {
        lane.parameter.max;
      } else {
        value;
      };
    }, lane.values)
  };
};

let randomAbsolute = (lane) => {
  ...lane,
  values: Array.map(_ => lane.parameter.randomValue(), lane.values)
};

let randomRelative = (delta, lane) => {
  ...lane,
  values: Array.map(value => lane.parameter.randomValueRelative(delta, value), lane.values)
};

let getParameter = (lane) => {
  lane.parameter;
};

let setParameter = (parameter, lane) => {
  let newLane = {
    ...lane,
    parameter
  };

  map((x, _, _) => x, newLane);
};
