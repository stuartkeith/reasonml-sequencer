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

let advance = (subTicks, loopAfterIndex, sync, t) => {
  switch (sync) {
    | Sync(tick) => {
      let index = (tick / subTicks) mod (loopAfterIndex + 1);
      let subIndex = tick mod subTicks;

      {
        index,
        subIndex
      };
    }
    | NoSync => {
      let nextSubIndex = t.subIndex + 1;

      if (nextSubIndex >= subTicks) {
        let nextIndex = (t.index + 1) mod (loopAfterIndex + 1);

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

let merge = (incomingSubTicks, incomingLoopAfterIndex, existing) => {
  // we want to keep the existing timing information, if possible.
  // it may not be possible because:
  // - the existing index exceeds the incoming limit. in that case, restart.
  if (existing.index > incomingLoopAfterIndex) {
    default;
  // - the existing subIndex exceeds the incoming limit. in that case, advance.
  } else if (existing.subIndex >= incomingSubTicks) {
    advance(incomingSubTicks, incomingLoopAfterIndex, NoSync, existing);
  } else {
    existing;
  };
};

let index = (loopAfterIndex, t) => {
  t.index mod (loopAfterIndex + 1);
};
