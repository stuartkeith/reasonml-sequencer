type sync =
  | Sync(int)
  | NoSync;

type t('a, 'b);

type values('a);

let create:(Parameter.t('a, 'b), int, int) => t('a, 'b);
let restart:(t('a, 'b)) => t('a, 'b);
let reset:(t('a, 'b)) => t('a, 'b);
let value:(t('a, 'b)) => 'a;
let advance:(sync, t('a, 'b)) => t('a, 'b);
let loopAfterIndex:(t('a, 'b)) => int;
let subTicks:(t('a, 'b)) => int;
let setSubTicks:(int, t('a, 'b)) => (t('a, 'b));
let visualIndex:(t('a, 'b)) => int;
let values:(t('a, 'b)) => values('a);
let setLoopAfterIndex:(int, t('a, 'b)) => t('a, 'b);
let randomLoopAfterIndex:(t('a, 'b)) => t('a, 'b);
let setValue:(values('a), int, 'a) => values('a);
let setValues:(values('a), t('a, 'b)) => t('a, 'b);
let mapTransform:(('a, 'a, 'a) => 'a, t('a, 'b)) => t('a, 'b);
let mapi:((int, 'a) => 'b, values('a)) => array('b);
let merge:(t('a, 'b), t('a, 'b)) => t('a, 'b);
let randomAbsolute:(t('a, 'b)) => t('a, 'b);
let randomRelative:('a, t('a, 'b)) => t('a, 'b);
let getParameter:(t('a, 'b)) => Parameter.t('a, 'b);
let setParameter:(Parameter.t('a, 'b), t('a, 'b)) => t('a, 'b);
let length:(values('a)) => int;
let get:(int, values('a)) => 'a;
