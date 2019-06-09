type values;

type index = int;
type length = int;

type valuesUpdate = {
  index,
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
  defaultValue: (SynthParameters.globalParameters) => 'a,
  randomValueAbsolute: (SynthParameters.globalParameters, array('a)) => array('a),
  randomValueRelative: (SynthParameters.globalParameters, array('a)) => array('a)
};

type updateSynthParametersFn('a) = (SynthParameters.parameters, 'a) => SynthParameters.parameters;
type toStringFn('a) = ('a) => string;

type valueConverter;

let createValueConverter: (valueConverterFunctions('a), updateSynthParametersFn('a), toStringFn('a)) => valueConverter;

let defaultValues: (SynthParameters.globalParameters, valueConverter, length) => values;

let randomValuesAbsolute: (SynthParameters.globalParameters, valueConverter, values) => values;

let randomValuesRelative: (SynthParameters.globalParameters, valueConverter, values) => values;

let mapValues: (SynthParameters.globalParameters, valueConverter, (index, value) => 'a, values) => array('a);

let getValuesAt: (SynthParameters.globalParameters, valueConverter, index, values) => float;

let updateValues: (SynthParameters.globalParameters, valueConverter, values, index, float) => values;

let valuesLength: (values) => length;

let updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, index, values, valueConverter) => SynthParameters.parameters;
