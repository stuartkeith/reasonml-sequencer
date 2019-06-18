type sync =
  | Sync(int)
  | NoSync;

type t;

let default: t;

let advance: (int, int, sync, t) => t;

let merge: (int, int, t) => t;

let index: (t) => int;
