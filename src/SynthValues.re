type values = array(float);

type value = {
  label: string,
  number: float
};

type valueConverterFunctions('a) = {
  defaultValue: (SynthParameters.globalParameters) => 'a,
  fromFloat: (SynthParameters.globalParameters, float) => 'a,
  toFloat: (SynthParameters.globalParameters, 'a) => float,
  randomValueAbsolute: (SynthParameters.globalParameters, 'a) => 'a,
  randomValueRelative: (SynthParameters.globalParameters, 'a) => 'a
};

type updateSynthParametersFn('a) = (SynthParameters.parameters, 'a) => SynthParameters.parameters;
type toStringFn('a) = ('a) => string;

type valueConverter = {
  defaultFloat: (SynthParameters.globalParameters) => float,
  randomValueAbsoluteFloat: (SynthParameters.globalParameters, float) => float,
  randomValueRelativeFloat: (SynthParameters.globalParameters, float) => float,
  quantiseFloat: (SynthParameters.globalParameters, float) => value,
  updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, float) => SynthParameters.parameters
};

let createValueConverter = (valueConverterFunctions, updateSynthParameters, toString) => {
  defaultFloat: (globalParameters) => {
    let defaultValue = valueConverterFunctions.defaultValue(globalParameters);

    valueConverterFunctions.toFloat(globalParameters, defaultValue);
  },
  randomValueAbsoluteFloat: (globalParameters, value) => {
    let actualValue = valueConverterFunctions.fromFloat(globalParameters, value);
    let randomValue = valueConverterFunctions.randomValueAbsolute(globalParameters, actualValue);

    valueConverterFunctions.toFloat(globalParameters, randomValue);
  },
  randomValueRelativeFloat: (globalParameters, value) => {
    let actualValue = valueConverterFunctions.fromFloat(globalParameters, value);
    let randomValue = valueConverterFunctions.randomValueRelative(globalParameters, actualValue);

    valueConverterFunctions.toFloat(globalParameters, randomValue);
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
  Array.map((value) => {
    valueConverter.randomValueAbsoluteFloat(globalParameters, value);
  }, values);
};

let randomValuesRelative = (globalParameters, valueConverter, values) => {
  Array.map((value) => {
    valueConverter.randomValueRelativeFloat(globalParameters, value);
  }, values);
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
