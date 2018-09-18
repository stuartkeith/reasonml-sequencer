type t;

let create:(int, int, int, int) => t;
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
let randomLoopAfterIndex:(t) => t;
let setValue:(int, int, bool, t) => t;
let map:((int, int, int) => int, t) => t;
let setMax:(int, t) => t;
