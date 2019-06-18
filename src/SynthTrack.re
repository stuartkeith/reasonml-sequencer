type t = {
  id: Id.t,
  label: string,
  values: SynthValues.values,
  valueConverter: SynthValues.valueConverter,
  loopLength: int,
  timing: Timing.t
};
