type sync =
  | Sync(int)
  | NoSync;

type t = {
  index: int,
  subIndex: int,
  subTicks: int
};

let start = {
  index: 0,
  subIndex: 0,
  subTicks: 1
};

let restart = (t) => {
  ...t,
  index: 0,
  subIndex: 0
};

let advance = (loopLength, sync, t) => {
  if (loopLength === 0) {
    raise(Invalid_argument("loopLength === 0"));
  };

  switch (sync) {
    | Sync(tick) => {
      let index = mod((tick / t.subTicks), loopLength);
      let subIndex = mod(tick, t.subTicks);

      {
        ...t,
        index,
        subIndex
      };
    }
    | NoSync => {
      let nextSubIndex = t.subIndex + 1;

      if (nextSubIndex >= t.subTicks) {
        let nextIndex = mod((t.index + 1), loopLength);

        {
          ...t,
          index: nextIndex,
          subIndex: 0
        };
      } else {
        {
          ...t,
          index: t.index,
          subIndex: nextSubIndex
        }
      };
    }
  };
};

let merge = (incomingLoopLength, incoming, existing) => {
  // we want to keep the existing timing information, if possible.
  // it may not be possible because:
  // - the existing index exceeds the incoming limit. in that case, restart.
  if (existing.index >= incomingLoopLength) {
    restart(existing);
  // - the existing subIndex exceeds the incoming limit. in that case, advance.
  } else if (existing.subIndex >= incoming.subTicks) {
    advance(incomingLoopLength, NoSync, existing);
  } else {
    existing;
  };
};

let index = (t) => t.index;

let subIndex = (t) => t.subIndex;

let subTicks = (t) => t.subTicks;

let setSubTicks = (subTicks, t) => {
  if (subTicks === 0) {
    raise(Invalid_argument("subTicks === 0"));
  };

  {
    ...t,
    subTicks
  };
};

let isFirstTick = (t) => t.subIndex === 0;
