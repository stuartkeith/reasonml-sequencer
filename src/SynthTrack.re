type t = {
  id: Id.t,
  label: string,
  values: SynthValues.values,
  valueConverter: SynthValues.valueConverter,
  loopLength: int,
  subTicks: int,
  timing: Timing.t
};
