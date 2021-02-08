type values;

let emptyValues: values;

let length: (values) => int;

type floatConverter<'a> = {
  fromFloat: (float) => 'a,
  toFloat: ('a) => float
};

let fromArray: (floatConverter<'a>, array<'a>) => values;

type index = int;
type length = int;

type update = {
  index,
  value: float
};

type updateGroup = {
  globalParameters: SynthParameters.globalParameters,
  timing: Timing.t
};

type valueConverterConfig<'a> = {
  floatConverter: floatConverter<'a>,
  default: (length) => array<'a>,
  randomAbsolute: (array<'a>) => array<'a>,
  randomRelative: (array<'a>) => array<'a>,
  updateSynthParameters: (updateGroup, SynthParameters.parameters, 'a) => SynthParameters.parameters,
  toString: ('a) => string
};

type valueConverter = {
  defaultValues: (length) => values,
  randomValuesAbsolute: (values) => values,
  randomValuesRelative: (values) => values,
  mapValues: 'a . ((index, float, string) => 'a, values) => array<'a>,
  getValueAt: (index, values) => float,
  updateValues: (values, update) => values,
  updateSynthParameters: (updateGroup, SynthParameters.parameters, values) => SynthParameters.parameters
};

let createValueConverter: (valueConverterConfig<'a>) => valueConverter;
