open SynthParameters;

type values;

type index = int;
type length = int;

type update = {
  index,
  value: float
};

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

type valueConverter;

let createValueConverter: (valueConverterConfig('a)) => valueConverter;

let defaultValues: (length, globalParameters, valueConverter) => values;

let randomValuesAbsolute: (globalParameters, valueConverter, values) => values;

let randomValuesRelative: (globalParameters, valueConverter, values) => values;

let mapValues: (globalParameters, valueConverter, (index, float, string) => 'a, values) => array('a);

let getValueAt: (globalParameters, valueConverter, index, values) => float;

let updateValues: (globalParameters, valueConverter, values, update) => values;

let valuesLength: (values) => length;

let updateSynthParameters: (globalParameters, parameters, Timing.t, values, valueConverter) => parameters;
