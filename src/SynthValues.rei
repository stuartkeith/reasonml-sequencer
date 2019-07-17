type values;

type index = int;
type length = int;

type floatConverters('a) = {
  fromFloat: (SynthParameters.globalParameters, float) => 'a,
  toFloat: (SynthParameters.globalParameters, 'a) => float
};

type valueConverterConfig('a) = {
  floatConverters: floatConverters('a),
  default: (length, SynthParameters.globalParameters) => array('a),
  randomAbsolute: (SynthParameters.globalParameters, array('a)) => array('a),
  randomRelative: (SynthParameters.globalParameters, array('a)) => array('a),
  updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, Timing.t, 'a) => SynthParameters.parameters,
  toString: ('a) => string
};

type valueConverter;

let createValueConverter: (valueConverterConfig('a)) => valueConverter;

let defaultValues: (length, SynthParameters.globalParameters, valueConverter) => values;

let randomValuesAbsolute: (SynthParameters.globalParameters, valueConverter, values) => values;

let randomValuesRelative: (SynthParameters.globalParameters, valueConverter, values) => values;

let mapValues: (SynthParameters.globalParameters, valueConverter, (index, float, string) => 'a, values) => array('a);

let getValueAt: (SynthParameters.globalParameters, valueConverter, index, values) => float;

let updateValues: (SynthParameters.globalParameters, valueConverter, values, index, float) => values;

let valuesLength: (values) => length;

let updateSynthParameters: (SynthParameters.globalParameters, SynthParameters.parameters, Timing.t, values, valueConverter) => SynthParameters.parameters;
