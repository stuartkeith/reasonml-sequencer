type sync =
  | Sync(int)
  | NoSync;

type t = {
  index: int,
  subIndex: int
};

let default = {
  index: 0,
  subIndex: 0
};

let advance = (subTicks, loopLength, sync, t) => {
  switch (sync) {
    | Sync(tick) => {
      let index = (tick / subTicks) mod loopLength;
      let subIndex = tick mod subTicks;

      {
        index,
        subIndex
      };
    }
    | NoSync => {
      let nextSubIndex = t.subIndex + 1;

      if (nextSubIndex >= subTicks) {
        let nextIndex = (t.index + 1) mod loopLength;

        {
          index: nextIndex,
          subIndex: 0
        };
      } else {
        {
          index: t.index,
          subIndex: nextSubIndex
        }
      };
    }
  };
};

let merge = (incomingSubTicks, incomingLoopLength, existing) => {
  // we want to keep the existing timing information, if possible.
  // it may not be possible because:
  // - the existing index exceeds the incoming limit. in that case, restart.
  if (existing.index >= incomingLoopLength) {
    default;
  // - the existing subIndex exceeds the incoming limit. in that case, advance.
  } else if (existing.subIndex >= incomingSubTicks) {
    advance(incomingSubTicks, incomingLoopLength, NoSync, existing);
  } else {
    existing;
  };
};

let index = (incomingLoopLength, t) => {
  t.index mod incomingLoopLength;
};
