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
  defaultValue: (globalParameters) => defaultValue(globalParameters, getArray(globalParameters)),
  randomValueAbsolute: ((globalParameters, values) => {
    let array = getArray(globalParameters);

    Array.map((_) => Utils.randomArrayValue(array), values);
  }),
  randomValueRelative: (globalParameters, values) => {
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

let chords = [|
  ("None", [||]),
  ("Maj", [|4, 7|]),
  ("Min", [|3, 7|]),
  ("Dom", [|3, 6|]),
  ("Maj7", [|4, 7, 11|]),
  ("Min7", [|3, 7, 10|]),
  ("Dom7", [|4, 7, 10|]),
  ("Sus2", [|2, 7|]),
  ("Sus4", [|5, 7|]),
  ("Aug", [|4, 8|])
|];

let default = (globalParameters) => {
  open SynthTrack;

  let create = (label, valueConverter) => {
    let values = SynthValues.defaultValues(globalParameters, valueConverter, 16);

    {
      id: Id.create(),
      label,
      values,
      valueConverter,
      loopAfterIndex: 7,
      subTicks: 1,
      timing: Timing.default
    };
  };

  [
    create("Octave", SynthValues.createValueConverter(
      {
        floatFns: intFloatFns(-3, 2),
        defaultValue: (_) => 0,
        randomValueAbsolute: randomIntAbsolute(-3, 2),
        randomValueRelative: randomIntRelative(-3, 2, 1)
      },
      (parameters, value) => {
        ...parameters,
        note: parameters.note + (value * 12)
      },
      intToPlusMinus
    )),
    create("Pitch", SynthValues.createValueConverter(
      mapArray(
        (globalParameters) => globalParameters.scale,
        (_globalParameters, array) => array[0],
        3
      ),
      (parameters, value) => {
        ...parameters,
        note: parameters.note + value
      },
      intToPlusMinus
    )),
    create("Gain", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 1.0),
        defaultValue: (_) => 1.0,
        randomValueAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValueRelative: randomFloatRelative(0.0, 1.0, 0.2)
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
        defaultValue: (_) => 0.0,
        randomValueAbsolute: randomFloatAbsolute(-1.0, 1.0),
        randomValueRelative: randomFloatRelative(-1.0, 1.0, 0.2)
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
        defaultValue: (_) => 1.0,
        randomValueAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValueRelative: randomFloatRelative(0.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        chance: parameters.chance *. value
      },
      floatToPercentageString
    )),
    create("Offset", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 0.25),
        defaultValue: (_) => 0.0,
        randomValueAbsolute: randomFloatAbsolute(0.0, 0.25),
        randomValueRelative: randomFloatRelative(0.0, 0.25, 0.1)
      },
      (parameters, value) => {
        ...parameters,
        offset: parameters.offset +. value
      },
      floatToPercentageString
    )),
    create("Length", SynthValues.createValueConverter(
      {
        floatFns: floatFloatFns(0.0, 2.0),
        defaultValue: (_) => 1.0,
        randomValueAbsolute: randomFloatAbsolute(0.0, 2.0),
        randomValueRelative: randomFloatRelative(0.0, 2.0, 0.2)
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
        defaultValue: (_) => 1.0,
        randomValueAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomValueRelative: randomFloatRelative(0.0, 1.0, 0.2)
      },
      (parameters, value) => {
        ...parameters,
        filter: parameters.filter *. value
      },
      floatToPercentageString
    )),
    create("Chord", SynthValues.createValueConverter(
      mapArray((_) => chords, (_, array) => array[0], 3),
      (parameters, (_, value)) => {
        ...parameters,
        chord: value
      },
      ((label, _)) => label
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
          timing: Timing.merge(incomingSynthTrack.subTicks, incomingSynthTrack.loopAfterIndex, existingSynthTrack.timing)
        };
      }
      | [] => incomingSynthTrack;
    }
  }, incomingSynthTracks);
};
