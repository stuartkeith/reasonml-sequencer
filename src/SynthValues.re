type values = array(float);

type index = int;
type length = int;

type valuesUpdate = {
  index: int,
  value: float
};

type value = {
  label: string,
  number: float
};

type floatFns('a) = {
  fromFloat: (SynthParameters.globalParameters, float) => 'a,
  toFloat: (SynthParameters.globalParameters, 'a) => float
};

type valueConverterFunctions('a) = {
  floatFns: floatFns('a),
  defaultValues: (length, SynthParameters.globalParameters) => array('a),
  randomValueAbsolute: (SynthParameters.globalParameters, array('a)) => array('a),
  randomValueRelative: (SynthParameters.globalParameters, array('a)) => array('a)
};

type updateSynthParametersFn('a) = (SynthParameters.parameters, 'a) => SynthParameters.parameters;
type toStringFn('a) = ('a) => string;

type valueConverter = {
  defaultFloat: (length, SynthParameters.globalParameters) => array(float),
  quantiseFloat: (SynthParameters.globalParameters, float) => value,
  randomValueAbsoluteFloat: (SynthParameters.globalParameters, array(float)) => array(float),
  randomValueRelativeFloat: (SynthParameters.globalParameters, array(float)) => array(float),
  updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, float) => SynthParameters.parameters
};

let createValueConverter = (valueConverterFunctions, updateSynthParameters, toString) => {
  defaultFloat: (length, globalParameters) => {
    valueConverterFunctions.defaultValues(length, globalParameters)
      |> Array.map(valueConverterFunctions.floatFns.toFloat(globalParameters));
  },
  randomValueAbsoluteFloat: (globalParameters, values) => {
    values
      |> Array.map(valueConverterFunctions.floatFns.fromFloat(globalParameters))
      |> valueConverterFunctions.randomValueAbsolute(globalParameters)
      |> Array.map(valueConverterFunctions.floatFns.toFloat(globalParameters));
  },
  randomValueRelativeFloat: (globalParameters, values) => {
    values
      |> Array.map(valueConverterFunctions.floatFns.fromFloat(globalParameters))
      |> valueConverterFunctions.randomValueRelative(globalParameters)
      |> Array.map(valueConverterFunctions.floatFns.toFloat(globalParameters));
  },
  quantiseFloat: (globalParameters, value) => {
    let actualValue = valueConverterFunctions.floatFns.fromFloat(globalParameters, value);
    let label = toString(actualValue);
    let floatValue = valueConverterFunctions.floatFns.toFloat(globalParameters, actualValue);

    {
      label,
      number: floatValue
    };
  },
  updateSynthParameters: (globalParameters, parameters, value) => {
    let value = valueConverterFunctions.floatFns.fromFloat(globalParameters, value);

    updateSynthParameters(parameters, value);
  }
};

let defaultValues = (length, globalParameters, valueConverter) => {
  valueConverter.defaultFloat(length, globalParameters);
};

let randomValuesAbsolute = (globalParameters, valueConverter, values) => {
  valueConverter.randomValueAbsoluteFloat(globalParameters, values);
};

let randomValuesRelative = (globalParameters, valueConverter, values) => {
  valueConverter.randomValueRelativeFloat(globalParameters, values);
};

let mapValues = (globalParameters, valueConverter, fn, values) => {
  Array.mapi((index, value) => {
    fn(index, valueConverter.quantiseFloat(globalParameters, value));
  }, values);
};

let getValuesAt = (globalParameters, valueConverter, index, values) => {
  valueConverter.quantiseFloat(globalParameters, values[index]).number;
};

let updateValues = (globalParameters, valueConverter, values, index, value) => {
  // don't store the raw value - store the value as interpreted by
  // the valueConverter instead.
  let actualValue = valueConverter.quantiseFloat(globalParameters, value);

  if (actualValue.number === values[index]) {
    values;
  } else {
    let newArray = Array.copy(values);

    newArray[index] = actualValue.number;

    newArray;
  };
};

let valuesLength = Array.length;

let updateSynthParameters = (globalParameters, parameters, index, values, valueConverter) => {
  let value = values[index];

  valueConverter.updateSynthParameters(globalParameters, parameters, value);
};
