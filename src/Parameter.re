type t('a, 'b) = {
  default:'a,
  min: 'a,
  max: 'a,
  value: 'b,
  randomValue: unit => 'a,
  randomValueRelative: ('a, 'a) => 'a,
  toFloat: 'a => float,
  fromFloat: float => 'a,
  toString: 'a => string
};

let createInt = (default, min, max) => {
  default,
  min,
  max,
  value: (),
  randomValue: () => min + Random.int(max - min + 1),
  randomValueRelative: (delta, value) => {
    let deltaMin = Pervasives.max(min, value - delta);
    let deltaMax = Pervasives.min(max, value + delta);

    deltaMin + Random.int(deltaMax - deltaMin + 1);
  },
  toFloat: (value) => float_of_int(value - min) /. float_of_int(max - min),
  fromFloat: (value) => min + int_of_float(float_of_int(max - min) *. value),
  toString: string_of_int
};

let createFloat = (default, min, max) => {
  default,
  min,
  max,
  value: (),
  randomValue: () => Random.float(1.),
  randomValueRelative: (delta, value) => {
    let deltaMin = Pervasives.max(min, value -. delta);
    let deltaMax = Pervasives.min(max, value +. delta);

    deltaMin +. Random.float(deltaMax -. deltaMin);
  },
  toFloat: (value) => (value -. min) /. (max -. min),
  fromFloat: (value) => min +. ((max -. min) *. value),
  toString: (value) => Utils.round(value *. 100.0)
};

let createScale = (scale) => {
  let max = Scales.max(scale);

  {
    default: 0,
    min: 0,
    max,
    value: scale,
    randomValue: () => Random.int(max + 1),
    randomValueRelative: (delta, value) => {
      let deltaMin = Pervasives.max(0, value - delta);
      let deltaMax = Pervasives.min(max, value + delta);

      deltaMin + Random.int(deltaMax - deltaMin + 1);
    },
    toFloat: (value) => float_of_int(value) /. float_of_int(max),
    fromFloat: (value) => int_of_float(value *. float_of_int(max)),
    toString: string_of_int
  };
};
