type values = array<float>;

let emptyValues = [];

let length = Array.length;

type floatConverter<'a> = {
  fromFloat: (float) => 'a,
  toFloat: ('a) => float
};

let fromArray = (floatConverter, array) => {
  Array.map(floatConverter.toFloat, array);
};

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

let createValueConverter = (valueConverterConfig) => {
  let floatConverter = valueConverterConfig.floatConverter;

  {
    defaultValues: (length) => {
      valueConverterConfig.default(length) |> fromArray(floatConverter);
    },
    randomValuesAbsolute: (values) => {
      values
        |> Array.map(floatConverter.fromFloat)
        |> valueConverterConfig.randomAbsolute
        |> fromArray(floatConverter);
    },
    randomValuesRelative: (values) => {
      values
        |> Array.map(floatConverter.fromFloat)
        |> valueConverterConfig.randomRelative
        |> fromArray(floatConverter);
    },
    mapValues: (fn, values) => {
      Array.mapi((index, value) => {
        let realValue = floatConverter.fromFloat(value);
        let label = valueConverterConfig.toString(realValue);
        let floatValue = floatConverter.toFloat(realValue);

        fn(index, floatValue, label);
      }, values);
    },
    getValueAt: (index, values) => {
      floatConverter.fromFloat(values[index]) |> floatConverter.toFloat;
    },
    updateValues: (values, update) => {
      let existingValue = floatConverter.fromFloat(values[update.index]);
      let newValue = floatConverter.fromFloat(update.value);

      if (existingValue === newValue) {
        values;
      } else {
        Array.mapi((index, value) => {
          if (update.index === index) {
            floatConverter.toFloat(newValue);
          } else {
            value;
          };
        }, values);
      };
    },
    updateSynthParameters: (updateGroup, parameters, values) => {
      let index = Timing.index(updateGroup.timing);
      let value = floatConverter.fromFloat(values[index]);

      valueConverterConfig.updateSynthParameters(updateGroup, parameters, value);
    }
  }
};
