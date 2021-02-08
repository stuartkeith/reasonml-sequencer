type sync =
  | Sync(int)
  | NoSync;

type t;

let start: t;

let restart: (t) => t;

let advance: (int, sync, t) => t;

let merge: (int, t, t) => t;

let index: (t) => int;

let subIndex: (t) => int;

let subTicks: (t) => int;

let setSubTicks: (int, t) => t;

let isFirstTick: (t) => bool;
