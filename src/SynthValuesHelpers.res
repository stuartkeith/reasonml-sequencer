open SynthValues;

let intRangeConverters = (min, max) => {
  fromFloat: (value) => {
    let range = -1 + Js.Math.ceil(value *. float_of_int(max - min + 1));

    min + Pervasives.max(0, range);
  },
  toFloat: (value) => {
    float_of_int(value - min + 1) /. float_of_int(max - min + 1);
  }
};

let randomIntAbsolute = (min, max) => {
  (values) => {
    Array.map((_) => Utils.randomInt(min, max), values);
  };
};

let randomIntRelative = (min, max, randomRelativeRange) => {
  (values) => {
    Array.map((value) => {
      let deltaMin = Pervasives.max(min, value - randomRelativeRange);
      let deltaMax = Pervasives.min(max, value + randomRelativeRange);

      Utils.randomInt(deltaMin, deltaMax);
    }, values);
  };
};

let floatRangeConverters = (min, max) => {
  fromFloat: (value) =>{
    min +. ((max -. min) *. value);
  },
  toFloat: (value) => {
    (value -. min) /. (max -. min);
  }
};

let randomFloatAbsolute = (min, max) => {
  (values) => {
    Array.map((_) => {
      Utils.randomFloat(min, max);
    }, values);
  };
};

let randomFloatRelative = (min, max, randomRelativeRange) => {
  (values) => {
    Array.map((value) => {
      let deltaMin = Pervasives.max(min, value -. randomRelativeRange);
      let deltaMax = Pervasives.min(max, value +. randomRelativeRange);

      Utils.randomFloat(deltaMin, deltaMax);
    }, values);
  };
};

let optionalArrayConverters = (array) => {
  // use 1-based indices, with 1 as None, then convert when needed.
  fromFloat: (value) => {
    let index = max(1, Js.Math.ceil(value *. float_of_int(Array.length(array) + 1)));

    if (index === 1) {
      None;
    } else {
      Some(array[index - 2]);
    };
  },
  toFloat: (value) => {
    let index = switch (value) {
      | None => 1
      | Some(value) => 2 + Utils.getArrayIndex(array, value, 0)
    };

    float_of_int(index) /. float_of_int(Array.length(array) + 1);
  }
};

let intToPlusMinus = value => {
  (value >= 0 ? "+" : "") ++ string_of_int(value);
};

let floatToPercentageString = (value) => {
  Js.Float.toString(Js.Math.round(value *. 100.0)) ++ "%";
};

let createPitch = (default) => {
  // pitches are stored as options.
  // use 1-based indices, with 1 as None, then convert when needed.
  (scale) => {
    floatConverter: optionalArrayConverters(scale),
    default: (length) => default(length, scale),
    randomAbsolute: ((values) => {
      let array = scale;
      let chance = 0.65 +. Random.float(0.2);

      Array.map(_ => {
        if (Random.float(1.0) <= chance) {
          Some(Utils.randomArrayValue(array));
        } else {
          None;
        }
      }, values);
    }),
    randomRelative: (values) => {
      let array = scale;
      let randomRelativeRange = 2;

       Array.map((value) => {
        let index = switch (value) {
          | None => 0
          | Some(value) => Utils.getArrayIndex(array, value, 0) + 1
        };

        let deltaMin = Pervasives.max(0, index - randomRelativeRange);
        let deltaMax = Pervasives.min(Array.length(array), index + randomRelativeRange);

        let randomIndex = Utils.randomInt(deltaMin, deltaMax);

        if (randomIndex === 0) {
          None;
        } else {
          Some(array[randomIndex - 1]);
        };
      }, values);
    },
    updateSynthParameters: ({ globalParameters, timing }, parameters, value) => {
      ...parameters,
      notes: {
        if (globalParameters.repeatNotesEverySubTick || Timing.isFirstTick(timing)) {
          switch (value) {
            | Some(value) => Array.append(parameters.notes, [value])
            | None => parameters.notes
          };
        } else {
          parameters.notes;
        }
      }
    },
    toString: (value) => {
      switch (value) {
        | Some(value) => string_of_int(value + 1)
        | None => "-"
      };
    }
  };
};

let octave = {
  floatConverter: intRangeConverters(-3, 2),
  default: (length) => Array.make(length, 0),
  randomAbsolute: randomIntAbsolute(-3, 2),
  randomRelative: randomIntRelative(-3, 2, 1),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    transpose: parameters.transpose + (value * 12)
  },
  toString: intToPlusMinus
};

let pitchWithFirstNote = createPitch((length, scale) => {
  let array = Array.make(length, None);

  array[0] = Some(Utils.randomArrayValue(scale));

  array;
});

let pitch = createPitch((length, _array) => {
  Array.make(length, None);
});

let gain = {
  floatConverter: floatRangeConverters(0.0, 1.0),
  default: (length) => Array.make(length, 1.0),
  randomAbsolute: randomFloatAbsolute(0.2, 1.0),
  randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    gain: parameters.gain *. value
  },
  toString: floatToPercentageString
};

let pan = {
  floatConverter: floatRangeConverters(-1.0, 1.0),
  default: (length) => Array.make(length, 0.0),
  randomAbsolute: randomFloatAbsolute(-1.0, 1.0),
  randomRelative: randomFloatRelative(-1.0, 1.0, 0.2),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    pan: parameters.pan +. value
  },
  toString: floatToPercentageString
};

let chance = {
  floatConverter: floatRangeConverters(0.0, 1.0),
  default: (length) => Array.make(length, 1.0),
  randomAbsolute: randomFloatAbsolute(0.0, 1.0),
  randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    chance: parameters.chance *. value
  },
  toString: floatToPercentageString
};

let length = {
  floatConverter: floatRangeConverters(0.0, 2.0),
  default: (length) => Array.make(length, 1.0),
  randomAbsolute: randomFloatAbsolute(0.0, 2.0),
  randomRelative: randomFloatRelative(0.0, 2.0, 0.2),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    length: parameters.length *. value
  },
  toString: floatToPercentageString
};

let filter = {
  floatConverter: floatRangeConverters(0.0, 1.0),
  default: (length) => Array.make(length, 1.0),
  randomAbsolute: randomFloatAbsolute(0.0, 1.0),
  randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
  updateSynthParameters: (_group, parameters, value) => {
    ...parameters,
    filter: parameters.filter *. value
  },
  toString: floatToPercentageString
};
