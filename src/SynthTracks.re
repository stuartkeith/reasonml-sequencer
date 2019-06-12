let intFloatFns = (min, max) => SynthValues.{
  fromFloat: (_globalParameters, value) => min + int_of_float(float_of_int(max - min) *. value),
  toFloat: (_globalParameters, value) => float_of_int(value - min) /. float_of_int(max - min)
};

let randomIntAbsolute = (min, max) => (_globalParameters, values) => Array.map((_) => Utils.randomInt(min, max), values);

let randomIntRelative = (min, max, randomRelativeRange) => (_globalParameters, values) => {
  Array.map((value) => {
    let deltaMin = Pervasives.max(min, value - randomRelativeRange);
    let deltaMax = Pervasives.min(max, value + randomRelativeRange);

    Utils.randomInt(deltaMin, deltaMax);
  }, values);
};

let floatFloatFns = (min, max) => SynthValues.{
  fromFloat: (_globalParameters, value) => min +. ((max -. min) *. value),
  toFloat: (_globalParameters, value) => (value -. min) /. (max -. min)
};

let randomFloatAbsolute = (min, max) => (_globalParameters, values) => Array.map((_) => Utils.randomFloat(min, max), values);

let randomFloatRelative = (min, max, randomRelativeRange) => (_globalParameters, values) => {
  Array.map((value) => {
    let deltaMin = Pervasives.max(min, value -. randomRelativeRange);
    let deltaMax = Pervasives.min(max, value +. randomRelativeRange);

    Utils.randomFloat(deltaMin, deltaMax);
  }, values);
};

let mapArray = (getArray, defaultValue, randomRelativeRange) => SynthValues.{
  floatFns: {
    fromFloat: (globalParameters, value) => {
      let array = getArray(globalParameters);
      let index = int_of_float(value *. float_of_int(Array.length(array) - 1));

      array[index];
    },
    toFloat: (globalParameters, value) => {
      let array = getArray(globalParameters);
      let index = Utils.getArrayIndex(array, value, 0);

      float_of_int(index) /. float_of_int(Array.length(array) - 1);
    }
  },
  defaultValues: (length, globalParameters) => Array.make(length, defaultValue(globalParameters, getArray(globalParameters))),
  randomValuesAbsolute: ((globalParameters, values) => {
    let array = getArray(globalParameters);

    Array.map((_) => Utils.randomArrayValue(array), values);
  }),
  randomValuesRelative: (globalParameters, values) => {
    let array = getArray(globalParameters);

    Array.map((value) => {
      let index = Utils.getArrayIndex(array, value, 0);

      let deltaMin = Pervasives.max(0, index - randomRelativeRange);
      let deltaMax = Pervasives.min(Array.length(array) - 1, index + randomRelativeRange);

      let randomIndex = Utils.randomInt(deltaMin, deltaMax);

      array[randomIndex];
    }, values);
  },
};

let intToPlusMinus = value => {
  (value >= 0 ? "+" : "") ++ string_of_int(value);
};

let floatToPercentageString = (value) => {
  Js.Float.toString(Js.Math.round(value *. 100.0)) ++ "%";
};

let default = (globalParameters) => {
  open SynthTrack;

  let create = (label, valueConverter) => {
    let values = SynthValues.defaultValues(16, globalParameters, valueConverter);

    {
      id: Id.create(),
      label,
      values,
      valueConverter,
      loopLength: 8,
      subTicks: 1,
      timing: Timing.default
    };
  };

  [
    create("Octave", SynthValues.createValueConverter(
      {
        floatFns: intFloatFns(-3, 2),
        defaultValues: (length, _globalParameters) => Array.make(length, 0),
        randomValuesAbsolute: randomIntAbsolute(-3, 2),
        randomValuesRelative: randomIntRelative(-3, 2, 1)
      },
      (parameters, value) => {
        ...parameters,
        transpose: parameters.transpose + (value * 12)
      },
      intToPlusMinus
    )),
    create("Pitch 1", SynthValues.createValueConverter(
      mapArray(
        (globalParameters) => globalParameters.scale,
        (_globalParameters, array) => array[0],
        3
      ),
      (parameters, value) => {
        ...parameters,
        notes: {
          switch (Utils.findInArray(value, parameters.notes)) {
            | None => Array.append(parameters.notes, [|value|])
            | Some(_) => parameters.notes
          };
        }
      },
      (value) => intToPlusMinus(value + 1)
    )),
    create("Pitch 2", SynthValues.createValueConverter(
      mapArray(
        (globalParameters) => globalParameters.scale,
        (_globalParameters, array) => array[0],
        3
      ),
      (parameters, value) => {
        ...parameters,
        notes: {
          switch (Utils.findInArray(value, parameters.notes)) {
            | None => Array.append(parameters.notes, [|value|])
            | Some(_) => parameters.notes
          };
        }
      },
      (value) => intToPlusMinus(value + 1)
    )),
    create("Pitch 3", SynthValues.createValueConverter(
      mapArray(
        (globalParameters) => globalParameters.scale,
        (_globalParameters, array) => array[0],
        3
      ),
      (parameters, value) => {
        ...parameters,
        notes: {
          switch (Utils.findInArray(value, parameters.notes)) {
            | None => Array.append(parameters.notes, [|value|])
            | Some(_) => parameters.notes
          };
        }
      },
      (value) => intToPlusMinus(value + 1)
    )),
    create("Gain", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 1.0),
        defaultValues: (length, _globalParameters) => Array.make(length, 1.0),
        randomValuesAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValuesRelative: randomFloatRelative(0.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        gain: parameters.gain *. value
      },
      floatToPercentageString
    )),
    create("Pan", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(-1.0, 1.0),
        defaultValues: (length, _globalParameters) => Array.make(length, 0.0),
        randomValuesAbsolute: randomFloatAbsolute(-1.0, 1.0),
        randomValuesRelative: randomFloatRelative(-1.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        pan: parameters.pan +. value
      },
      floatToPercentageString
    )),
    create("Chance", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 1.0),
        defaultValues: (length, _globalParameters) => Array.make(length, 1.0),
        randomValuesAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValuesRelative: randomFloatRelative(0.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        chance: parameters.chance *. value
      },
      floatToPercentageString
    )),
    create("Length", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 2.0),
        defaultValues: (length, _globalParameters) => Array.make(length, 1.0),
        randomValuesAbsolute: randomFloatAbsolute(0.0, 2.0),
        randomValuesRelative: randomFloatRelative(0.0, 2.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        length: parameters.length *. value
      },
      floatToPercentageString
    )),
    create("Filter", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 1.0),
        defaultValues: (length, _globalParameters) => Array.make(length, 1.0),
        randomValuesAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValuesRelative: randomFloatRelative(0.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        filter: parameters.filter *. value
      },
      floatToPercentageString
    ))
  ];
};

let mapSynthTrackById = (id, fn, synthTracks) => {
  List.map((synthTrack:SynthTrack.t) => {
    if (Id.equals(synthTrack.id, id)) {
      fn(synthTrack);
    } else {
      synthTrack;
    }
  }, synthTracks);
};

let merge = (incomingSynthTracks, existingSynthTracks) => {
  let existingSynthTracksCurrent = ref(existingSynthTracks);

  List.map(incomingSynthTrack => {
    switch (existingSynthTracksCurrent^) {
      | [existingSynthTrack, ...existingSynthTracksRest] => {
        existingSynthTracksCurrent := existingSynthTracksRest;

        SynthTrack.{
          ...incomingSynthTrack,
          timing: Timing.merge(incomingSynthTrack.subTicks, incomingSynthTrack.loopLength, existingSynthTrack.timing)
        };
      }
      | [] => incomingSynthTrack;
    }
  }, incomingSynthTracks);
};
