let intFloatFns = (min, max) => SynthValues.{
  fromFloat: (_globalParameters, value) => {
    let range = -1 + Js.Math.ceil(value *. float_of_int(max - min + 1));

    min + Pervasives.max(0, range);
  },
  toFloat: (_globalParameters, value) => float_of_int(value - min + 1) /. float_of_int(max - min + 1)
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

let intToPlusMinus = value => {
  (value >= 0 ? "+" : "") ++ string_of_int(value);
};

let pitchValueConverter = (defaultValues) => SynthValues.createValueConverter(
  // pitches are stored as options.
  // use 1-based indices, with 1 as None, then convert when needed.
  {
    floatFns: {
      fromFloat: (globalParameters, value) => {
        let array = globalParameters.scale;
        let index = max(1, Js.Math.ceil(value *. float_of_int(Array.length(array) + 1)));

        if (index === 1) {
          None;
        } else {
          Some(array[index - 2]);
        };
      },
      toFloat: (globalParameters, value) => {
        let array = globalParameters.scale;

        let index = switch (value) {
          | None => 1
          | Some(value) => 2 + Utils.getArrayIndex(array, value, 0)
        };

        float_of_int(index) /. float_of_int(Array.length(array) + 1);
      }
    },
    defaultValues,
    randomValuesAbsolute: ((globalParameters, values) => {
      let array = globalParameters.scale;
      let chance = 0.65 +. Random.float(0.2);

      values
        |> Array.map(_ => {
          if (Random.float(1.0) <= chance) {
            Some(Utils.randomArrayValue(array));
          } else {
            None;
          }
        });
    }),
    randomValuesRelative: (globalParameters, values) => {
      let array = globalParameters.scale;
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
    }
  },
  (parameters, value) => {
    ...parameters,
    notes: {
      switch (value) {
        | Some(value) => switch (Utils.findInArray(value, parameters.notes)) {
          | None => Array.append(parameters.notes, [|value|])
          | Some(_) => parameters.notes
        }
        | None => parameters.notes
      };
    }
  },
  (value) => {
    switch (value) {
      | Some(value) => string_of_int(value + 1)
      | None => "-"
    };
  }
);

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
    create("Pitch 1", pitchValueConverter((length, globalParameters) => {
      let array = Array.make(length, None);

      array[0] = Some(Utils.randomArrayValue(globalParameters.scale));

      array;
    })),
    create("Pitch 2", pitchValueConverter((length, _globalParameters) => {
      Array.make(length, None);
    })),
    create("Pitch 3", pitchValueConverter((length, _globalParameters) => {
      Array.make(length, None);
    })),
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
