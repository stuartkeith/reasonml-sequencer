type t;

let create: (SynthValues.values) => t;

let merge: (t, t) => t;

let restart: (t) => t;

let advance: (Timing.sync, t) => t;

let randomAbsolute: (SynthValues.valueConverter, t) => t;

let randomRelative: (SynthValues.valueConverter, t) => t;

let loopLength: (t) => int;

let setLoopLength: (int, t) => t;

let reset: (SynthValues.valueConverter, t) => t;

let setSubTicks: (int, t) => t;

let values: (t) => SynthValues.values;

let setValues: (SynthValues.values, t) => t;

let applyUpdate: (SynthValues.valueConverter, SynthValues.values, SynthValues.update, t) => t;

let timing: (t) => Timing.t;
