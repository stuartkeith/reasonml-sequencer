open SynthParameters;

type values = array(float);

type index = int;
type length = int;

type floatConverters('a) = {
  fromFloat: (globalParameters, float) => 'a,
  toFloat: (globalParameters, 'a) => float
};

type valueConverterConfig('a) = {
  floatConverters: floatConverters('a),
  default: (length, globalParameters) => array('a),
  randomAbsolute: (globalParameters, array('a)) => array('a),
  randomRelative: (globalParameters, array('a)) => array('a),
  updateSynthParameters: (globalParameters, parameters, Timing.t, 'a) => parameters,
  toString: ('a) => string
};

type valueConverter = {
  defaultValues: (length, globalParameters) => array(float),
  valueToString: (globalParameters, float) => string,
  snapValue: (globalParameters, float) => float,
  randomAbsoluteValues: (globalParameters, array(float)) => array(float),
  randomRelativeValues: (globalParameters, array(float)) => array(float),
  updateSynthParameters: (globalParameters, parameters, Timing.t, float) => parameters
};

let createValueConverter = (valueConverterConfig) => {
  defaultValues: (length, globalParameters) => {
    valueConverterConfig.default(length, globalParameters)
      |> Array.map(valueConverterConfig.floatConverters.toFloat(globalParameters));
  },
  valueToString: (globalParameters, value) => {
    let actualValue = valueConverterConfig.floatConverters.fromFloat(globalParameters, value);

    valueConverterConfig.toString(actualValue);
  },
  snapValue: (globalParameters, value) => {
    valueConverterConfig.floatConverters.fromFloat(globalParameters, value)
      |> valueConverterConfig.floatConverters.toFloat(globalParameters);
  },
  randomAbsoluteValues: (globalParameters, values) => {
    values
      |> Array.map(valueConverterConfig.floatConverters.fromFloat(globalParameters))
      |> valueConverterConfig.randomAbsolute(globalParameters)
      |> Array.map(valueConverterConfig.floatConverters.toFloat(globalParameters));
  },
  randomRelativeValues: (globalParameters, values) => {
    values
      |> Array.map(valueConverterConfig.floatConverters.fromFloat(globalParameters))
      |> valueConverterConfig.randomRelative(globalParameters)
      |> Array.map(valueConverterConfig.floatConverters.toFloat(globalParameters));
  },
  updateSynthParameters: (globalParameters, parameters, timing, value) => {
    let value = valueConverterConfig.floatConverters.fromFloat(globalParameters, value);

    valueConverterConfig.updateSynthParameters(globalParameters, parameters, timing, value);
  }
};

let defaultValues = (length, globalParameters, valueConverter) => {
  valueConverter.defaultValues(length, globalParameters);
};

let randomValuesAbsolute = (globalParameters, valueConverter, values) => {
  valueConverter.randomAbsoluteValues(globalParameters, values);
};

let randomValuesRelative = (globalParameters, valueConverter, values) => {
  valueConverter.randomRelativeValues(globalParameters, values);
};

let mapValues = (globalParameters, valueConverter, fn, values) => {
  Array.mapi((index, value) => {
    let label = valueConverter.valueToString(globalParameters, value);

    fn(index, value, label);
  }, values);
};

let getValueAt = (globalParameters, valueConverter, index, values) => {
  valueConverter.snapValue(globalParameters, values[index]);
};

let updateValues = (globalParameters, valueConverter, values, index, value) => {
  // don't store the raw value - store the value as interpreted by
  // the valueConverter instead.
  let actualValue = valueConverter.snapValue(globalParameters, value);

  if (actualValue === values[index]) {
    values;
  } else {
    let newArray = Array.copy(values);

    newArray[index] = actualValue;

    newArray;
  };
};

let valuesLength = Array.length;

let updateSynthParameters = (globalParameters, parameters, timing, values, valueConverter) => {
  let index = Timing.index(timing);
  let value = values[index];

  valueConverter.updateSynthParameters(globalParameters, parameters, timing, value);
};
