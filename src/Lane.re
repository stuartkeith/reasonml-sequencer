type sync =
  | Sync(int)
  | NoSync;

type t('a, 'b) = {
  parameter: Parameter.t('a, 'b),
  values: array('a),
  subTicks: int,
  index: int,
  subIndex: int,
  visualIndex: int,
  loopAfterIndex: int
};

type values('a) = array('a);

let create = (parameter, subTicks, length) => {
  parameter,
  values: Array.make(length, parameter.default),
  subTicks,
  index: 0,
  subIndex: 0,
  visualIndex: 0,
  loopAfterIndex: length > 2 ? (length / 2) - 1 : 0
};

let advance = (sync, lane) => {
  let (index, subIndex) = switch (sync) {
    | Sync(tick) => {
      let index = (tick / lane.subTicks) mod (lane.loopAfterIndex + 1);
      let subIndex = tick mod lane.subTicks;

      (index, subIndex);
    }
    | NoSync => {
      let nextSubIndex = lane.subIndex + 1;

      if (nextSubIndex >= lane.subTicks) {
        let nextIndex = lane.index >= lane.loopAfterIndex ? 0 : lane.index + 1;

        (nextIndex, 0);
      } else {
        (lane.index, nextSubIndex);
      };
    }
  };

  {
    ...lane,
    index,
    subIndex,
    visualIndex: lane.index
  };
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
let subTicks = (lane) => lane.subTicks;
let visualIndex = (lane) => lane.visualIndex;
let values = (lane) => lane.values;

let setSubTicks = (subTicks, lane) => {
  ...lane,
  subTicks
};

let setLoopAfterIndex = (loopAfterIndex, lane) => {
  ...lane,
  loopAfterIndex
};

let randomLoopAfterIndex = (lane) => {
  ...lane,
  loopAfterIndex: Random.int(Array.length(lane.values))
};

let transform = (fn, values, parameter, lane) => {
  {
    ...lane,
    parameter,
    values: Array.map(value => {
      let value = fn(value, lane.parameter.min, lane.parameter.max);

      if (value < lane.parameter.min) {
        lane.parameter.min;
      } else if (value > lane.parameter.max) {
        lane.parameter.max;
      } else {
        value;
      };
    }, values)
  };
};

let mapTransform = (fn, lane) => transform(fn, lane.values, lane.parameter, lane);

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

let setParameter = (parameter, lane) => transform((x, _, _) => x, lane.values, parameter, lane);

let setValues = (values, lane) => transform((x, _, _) => x, values, lane.parameter, lane);

let merge = (source, target) => {
  let loopAfterIndex = min(target.loopAfterIndex, Array.length(source.values) - 1);

  let (index, subIndex, visualIndex) = switch (source, target) {
    | (_source, target) when target.index > loopAfterIndex => (0, 0, 0)
    | (source, target) when target.subIndex >= source.subTicks => {
      let nextIndex = target.index + 1;
      let nextIndex = nextIndex > loopAfterIndex ? 0 : nextIndex;

      (nextIndex, 0, nextIndex);
    }
    | (_source, target) => (target.index, target.subIndex, target.visualIndex)
  };

  {
    parameter: source.parameter,
    values: source.values,
    subTicks: source.subTicks,
    index,
    subIndex,
    visualIndex,
    loopAfterIndex
  };
};

let setValue = (values, index, value) => {
  let newArray = Array.copy(values);

  newArray[index] = value;

  newArray;
};

let mapi = Array.mapi;

let get = (index, values) => values[index];

let length = Array.length;
