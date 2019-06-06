type values;

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

type valueConverter;

let createValueConverter: (valueConverterFunctions('a), updateSynthParametersFn('a), toStringFn('a)) => valueConverter;

let defaultValues: (SynthParameters.globalParameters, valueConverter, int) => values;

let randomValuesAbsolute: (SynthParameters.globalParameters, valueConverter, values) => values;

let randomValuesRelative: (SynthParameters.globalParameters, valueConverter, values) => values;

let mapValues: (SynthParameters.globalParameters, valueConverter, (int, value) => 'a, values) => array('a);

let getValuesAt: (SynthParameters.globalParameters, valueConverter, int, values) => float;

let updateValues: (SynthParameters.globalParameters, valueConverter, values, int, float) => values;

let valuesLength: (values) => int;

let updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, int, values, valueConverter) => SynthParameters.parameters;
