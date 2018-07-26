type t;

type random =
  | Absolute
  | Relative(int)
  | Range(int, int);

let empty:(int, int, int) => t;
let restart:(t) => t;
let reset:(t) => t;
let value:(t) => int;
let max:(t) => int;
let min:(t) => int;
let advance:(t) => t;
let loopAfterIndex:(t) => int;
let visualIndex:(t) => int;
let values:(t) => array(int);
let setLoopAfterIndex:(int, t) => t;
let setValue:(int, int, bool, t) => t;
let randomise:(random, t) => t;
let setMax:(int, t) => t;
