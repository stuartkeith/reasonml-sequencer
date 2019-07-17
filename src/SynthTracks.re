let intRangeConverters = (min, max) => SynthValues.{
  fromFloat: (_globalParameters, value) => {
    let range = -1 + Js.Math.ceil(value *. float_of_int(max - min + 1));

    min + Pervasives.max(0, range);
  },
  toFloat: (_globalParameters, value) => {
    float_of_int(value - min + 1) /. float_of_int(max - min + 1);
  }
};

let randomIntAbsolute = (min, max) => {
  (_globalParameters, values) => {
    Array.map((_) => Utils.randomInt(min, max), values);
  };
};

let randomIntRelative = (min, max, randomRelativeRange) => {
  (_globalParameters, values) => {
    Array.map((value) => {
      let deltaMin = Pervasives.max(min, value - randomRelativeRange);
      let deltaMax = Pervasives.min(max, value + randomRelativeRange);

      Utils.randomInt(deltaMin, deltaMax);
    }, values);
  };
};

let floatRangeConverters = (min, max) => SynthValues.{
  fromFloat: (_globalParameters, value) =>{
    min +. ((max -. min) *. value);
  },
  toFloat: (_globalParameters, value) => {
    (value -. min) /. (max -. min);
  }
};

let randomFloatAbsolute = (min, max) => {
  (_globalParameters, values) => {
    Array.map((_) => {
      Utils.randomFloat(min, max);
    }, values);
  };
};

let randomFloatRelative = (min, max, randomRelativeRange) => {
  (_globalParameters, values) => {
    Array.map((value) => {
      let deltaMin = Pervasives.max(min, value -. randomRelativeRange);
      let deltaMax = Pervasives.min(max, value +. randomRelativeRange);

      Utils.randomFloat(deltaMin, deltaMax);
    }, values);
  };
};

let intToPlusMinus = value => {
  (value >= 0 ? "+" : "") ++ string_of_int(value);
};

let pitchValueConverter = (default) => SynthValues.createValueConverter(
  // pitches are stored as options.
  // use 1-based indices, with 1 as None, then convert when needed.
  {
    floatConverters: {
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
    default,
    randomAbsolute: ((globalParameters, values) => {
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
    randomRelative: (globalParameters, values) => {
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
    },
    updateSynthParameters: (globalParameters, parameters, timing, value) => {
      ...parameters,
      notes: {
        if (globalParameters.repeatNotesEverySubTick || Timing.isFirstTick(timing)) {
          switch (value) {
            | Some(value) => Array.append(parameters.notes, [|value|])
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
  }
);

let floatToPercentageString = (value) => {
  Js.Float.toString(Js.Math.round(value *. 100.0)) ++ "%";
};

type createArguments = {
  trackLabel: string,
  trackValueConverter: SynthValues.valueConverter
};

let default = (globalParameters) => {
  open SynthTrack;

  let create = ({ trackLabel, trackValueConverter }) => {
    let values = SynthValues.defaultValues(16, globalParameters, trackValueConverter);

    {
      id: Id.create(),
      label: trackLabel,
      values,
      valueConverter: trackValueConverter,
      loopLength: 8,
      timing: Timing.create(1)
    };
  };

  [
    create({
      trackLabel: "Octave",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: intRangeConverters(-3, 2),
        default: (length, _globalParameters) => Array.make(length, 0),
        randomAbsolute: randomIntAbsolute(-3, 2),
        randomRelative: randomIntRelative(-3, 2, 1),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          transpose: parameters.transpose + (value * 12)
        },
        toString: intToPlusMinus
      })
    }),
    create({
      trackLabel: "Pitch 1",
      trackValueConverter: pitchValueConverter((length, globalParameters) => {
        let array = Array.make(length, None);

        array[0] = Some(Utils.randomArrayValue(globalParameters.scale));

        array;
      })
    }),
    create({
      trackLabel: "Pitch 2",
      trackValueConverter: pitchValueConverter((length, _globalParameters) => {
        Array.make(length, None);
      })
    }),
    create({
      trackLabel: "Pitch 3",
      trackValueConverter: pitchValueConverter((length, _globalParameters) => {
        Array.make(length, None);
      })
    }),
    create({
      trackLabel: "Gain",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: floatRangeConverters(0.0, 1.0),
        default: (length, _globalParameters) => Array.make(length, 1.0),
        randomAbsolute: randomFloatAbsolute(0.2, 1.0),
        randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          gain: parameters.gain *. value
        },
        toString: floatToPercentageString
      })
    }),
    create({
      trackLabel: "Pan",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: floatRangeConverters(-1.0, 1.0),
        default: (length, _globalParameters) => Array.make(length, 0.0),
        randomAbsolute: randomFloatAbsolute(-1.0, 1.0),
        randomRelative: randomFloatRelative(-1.0, 1.0, 0.2),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          pan: parameters.pan +. value
        },
        toString: floatToPercentageString
      })
    }),
    create({
      trackLabel: "Chance",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: floatRangeConverters(0.0, 1.0),
        default: (length, _globalParameters) => Array.make(length, 1.0),
        randomAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          chance: parameters.chance *. value
        },
        toString: floatToPercentageString
      })
    }),
    create({
      trackLabel: "Length",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: floatRangeConverters(0.0, 2.0),
        default: (length, _globalParameters) => Array.make(length, 1.0),
        randomAbsolute: randomFloatAbsolute(0.0, 2.0),
        randomRelative: randomFloatRelative(0.0, 2.0, 0.2),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          length: parameters.length *. value
        },
        toString: floatToPercentageString
      })
    }),
    create({
      trackLabel: "Filter",
      trackValueConverter: SynthValues.createValueConverter({
        floatConverters: floatRangeConverters(0.0, 1.0),
        default: (length, _globalParameters) => Array.make(length, 1.0),
        randomAbsolute: randomFloatAbsolute(0.0, 1.0),
        randomRelative: randomFloatRelative(0.0, 1.0, 0.2),
        updateSynthParameters: (_globalParameters, parameters, _timing, value) => {
          ...parameters,
          filter: parameters.filter *. value
        },
        toString: floatToPercentageString
      })
    })
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
          timing: Timing.merge(incomingSynthTrack.loopLength, incomingSynthTrack.timing, existingSynthTrack.timing)
        };
      }
      | [] => incomingSynthTrack;
    }
  }, incomingSynthTracks);
};
