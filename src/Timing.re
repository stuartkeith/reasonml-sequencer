type sync =
  | Sync(int)
  | NoSync;

type t = {
  index: int,
  subIndex: int,
  subTicks: int
};

let create = (subTicks) => {
  index: 0,
  subIndex: 0,
  subTicks
};

let restart = (t) => {
  ...t,
  index: 0,
  subIndex: 0
};

let advance = (loopLength, sync, t) => {
  switch (sync) {
    | Sync(tick) => {
      let index = (tick / t.subTicks) mod loopLength;
      let subIndex = tick mod t.subTicks;

      {
        ...t,
        index,
        subIndex
      };
    }
    | NoSync => {
      let nextSubIndex = t.subIndex + 1;

      if (nextSubIndex >= t.subTicks) {
        let nextIndex = (t.index + 1) mod loopLength;

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

let subTicks = (t) => t.subTicks;

let setSubTicks = (subTicks, t) => {
  ...t,
  subTicks
};

let isFirstTick = (t) => t.subIndex === 0;
