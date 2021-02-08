type t = {
  values: SynthValues.values,
  loopLength: int,
  timing: Timing.t
};

let create = (values) => {
  if (SynthValues.length(values) === 0) {
    raise(Invalid_argument("values length === 0"));
  };

  {
    values,
    loopLength: max(1, SynthValues.length(values) / 2),
    timing: Timing.start
  };
};

let merge = (incomingSynthInstance, existingSynthInstance) => {
  ...incomingSynthInstance,
  timing: Timing.merge(incomingSynthInstance.loopLength, incomingSynthInstance.timing, existingSynthInstance.timing)
};

let restart = (synthInstance) => {
  ...synthInstance,
  timing: Timing.restart(synthInstance.timing)
};

let advance = (sync, synthInstance) => {
  ...synthInstance,
  timing: Timing.advance(synthInstance.loopLength, sync, synthInstance.timing)
};

let randomAbsolute = (valueConverter: SynthValues.valueConverter, synthInstance) => {
  ...synthInstance,
  values: valueConverter.randomValuesAbsolute(synthInstance.values),
  loopLength: Utils.randomInt(2, SynthValues.length(synthInstance.values))
};

let randomRelative = (valueConverter: SynthValues.valueConverter, synthInstance) => {
  ...synthInstance,
  values: valueConverter.randomValuesRelative(synthInstance.values),
  loopLength: Utils.randomInt(2, SynthValues.length(synthInstance.values))
};

let loopLength = (synthInstance) => synthInstance.loopLength;

let setLoopLength = (loopLength, synthInstance) => {
  if (loopLength <= 0) {
    raise(Invalid_argument("loop length <= 0"));
  };

  if (loopLength >= SynthValues.length(synthInstance.values)) {
    raise(Invalid_argument("loop length >= length of values"));
  };

  {
    ...synthInstance,
    loopLength
  };
};

let reset = (valueConverter: SynthValues.valueConverter, synthInstance) => {
  ...synthInstance,
  values: valueConverter.defaultValues(SynthValues.length(synthInstance.values))
};

let setSubTicks = (subTicks, synthInstance) => {
  ...synthInstance,
  timing: Timing.setSubTicks(subTicks, synthInstance.timing)
};

let values = (synthInstance) => {
  synthInstance.values
};

let setValues = (values, synthInstance) => {
  ...synthInstance,
  loopLength: min(synthInstance.loopLength, SynthValues.length(values)),
  values
};

let applyUpdate = (valueConverter: SynthValues.valueConverter, values, update, synthInstance) => {
  ...synthInstance,
  values: valueConverter.updateValues(values, update)
};

let timing = (synthInstance) => {
  synthInstance.timing;
};
