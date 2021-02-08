type t<'a> = {
  length: int,
  buffer: array<'a>,
  indexHead: int,
  indexTail: int,
  indexAccess: int
};

let create = (length, initialValue) => {
  length,
  buffer: Array.make(length + 1, initialValue),
  indexHead: 0,
  indexTail: 0,
  indexAccess: 0
}

let isEmpty = (t) => {
  t.indexHead == t.indexTail;
};

let read = (t) => {
  if (isEmpty(t)) {
    None;
  } else {
    Some(t.buffer[t.indexAccess])
  };
};

let pop = (t) => {
  if (isEmpty(t)) {
    t;
  } else {
    {
      ...t,
      indexHead: t.indexAccess,
      indexAccess: t.indexAccess == 0 ? t.length : t.indexAccess - 1
    };
  }
};

let incrementValue = (value, max) => value == max ? 0 : (value + 1);

let write = (value, t) => {
  t.buffer[t.indexHead] = value;

  let indexHead = incrementValue(t.indexHead, t.length);
  let indexTail = indexHead == t.indexTail ? incrementValue(t.indexTail, t.length) : t.indexTail;

  {
    ...t,
    indexHead,
    indexTail,
    indexAccess: t.indexHead
  };
};
