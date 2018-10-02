type sync =
  | Sync(int)
  | NoSync;

type t('a, 'b);

let create:(Parameter.t('a, 'b), int, int) => t('a, 'b);
let restart:(t('a, 'b)) => t('a, 'b);
let reset:(t('a, 'b)) => t('a, 'b);
let value:(t('a, 'b)) => 'a;
let advance:(sync, t('a, 'b)) => t('a, 'b);
let loopAfterIndex:(t('a, 'b)) => int;
let subTicks:(t('a, 'b)) => int;
let setSubTicks:(int, t('a, 'b)) => (t('a, 'b));
let visualIndex:(t('a, 'b)) => int;
let values:(t('a, 'b)) => array('a);
let setLoopAfterIndex:(int, t('a, 'b)) => t('a, 'b);
let randomLoopAfterIndex:(t('a, 'b)) => t('a, 'b);
let setValue:(int, 'a, bool, t('a, 'b)) => t('a, 'b);
let map:(('a, 'a, 'a) => 'a, t('a, 'b)) => t('a, 'b);
let randomAbsolute:(t('a, 'b)) => t('a, 'b);
let randomRelative:('a, t('a, 'b)) => t('a, 'b);
let getParameter:(t('a, 'b)) => Parameter.t('a, 'b);
let setParameter:(Parameter.t('a, 'b), t('a, 'b)) => t('a, 'b);
