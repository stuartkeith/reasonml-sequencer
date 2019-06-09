type values = array(float);

type valuesUpdate = {
  index: int,
  value: float
};

type value = {
  label: string,
  number: float
};

type valueConverterFunctions('a) = {
  defaultValue: (SynthParameters.globalParameters) => 'a,
  fromFloat: (SynthParameters.globalParameters, float) => 'a,
  toFloat: (SynthParameters.globalParameters, 'a) => float,
  randomValueAbsolute: (SynthParameters.globalParameters, array('a)) => array('a),
  randomValueRelative: (SynthParameters.globalParameters, array('a)) => array('a)
};

type updateSynthParametersFn('a) = (SynthParameters.parameters, 'a) => SynthParameters.parameters;
type toStringFn('a) = ('a) => string;

type valueConverter = {
  defaultFloat: (SynthParameters.globalParameters) => float,
  quantiseFloat: (SynthParameters.globalParameters, float) => value,
  randomValueAbsoluteFloat: (SynthParameters.globalParameters, array(float)) => array(float),
  randomValueRelativeFloat: (SynthParameters.globalParameters, array(float)) => array(float),
  updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, float) => SynthParameters.parameters
};

let createValueConverter = (valueConverterFunctions, updateSynthParameters, toString) => {
  defaultFloat: (globalParameters) => {
    let defaultValue = valueConverterFunctions.defaultValue(globalParameters);

    valueConverterFunctions.toFloat(globalParameters, defaultValue);
  },
  randomValueAbsoluteFloat: (globalParameters, values) => {
    values
      |> Array.map(valueConverterFunctions.fromFloat(globalParameters))
      |> valueConverterFunctions.randomValueAbsolute(globalParameters)
      |> Array.map(valueConverterFunctions.toFloat(globalParameters));
  },
  randomValueRelativeFloat: (globalParameters, values) => {
    values
      |> Array.map(valueConverterFunctions.fromFloat(globalParameters))
      |> valueConverterFunctions.randomValueRelative(globalParameters)
      |> Array.map(valueConverterFunctions.toFloat(globalParameters));
  },
  quantiseFloat: (globalParameters, value) => {
    let actualValue = valueConverterFunctions.fromFloat(globalParameters, value);
    let label = toString(actualValue);
    let floatValue = valueConverterFunctions.toFloat(globalParameters, actualValue);

    {
      label,
      number: floatValue
    };
  },
  updateSynthParameters: (globalParameters, parameters, value) => {
    let value = valueConverterFunctions.fromFloat(globalParameters, value);

    updateSynthParameters(parameters, value);
  }
};

let defaultValues = (globalParameters, valueConverter, length) => {
  let defaultValue = valueConverter.defaultFloat(globalParameters);

  Array.make(length, defaultValue);
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
