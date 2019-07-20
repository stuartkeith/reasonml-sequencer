open Jest;
open Expect;

let applyMultipleTimes = (count, fn, timing) => {
  let timing = ref(timing);

  for (_ in 1 to count) {
    timing := fn(timing^);
  }

  timing^;
};

describe("Timing", () => {
  test("should throw an exception on a loop length of zero", () => {
    expect(() => Timing.advance(0, NoSync, Timing.start)) |> toThrow;
  });

  test("should throw an exception on subTicks value of zero", () => {
    expect(() => Timing.setSubTicks(0, Timing.start)) |> toThrow;
  });

  test("should not advance the index to the loop length", () => {
    let result = Timing.advance(1, NoSync, Timing.start);

    expect(Timing.index(result)) |> toBe(0);
  });

  test("should only advance the index when it reaches the subTicks", () => {
    let timing = Timing.start
      |> Timing.setSubTicks(2)
      |> applyMultipleTimes(2, Timing.advance(8, NoSync));

    expect((
      Timing.index(timing),
      Timing.subIndex(timing)
    )) |> toEqual((1, 0));
  });

  test("should advance to the correct value when less than the loop length", () => {
    let existing = Timing.start
      |> applyMultipleTimes(4, Timing.advance(8, NoSync));

    expect((Timing.index(existing), Timing.subIndex(existing))) |> toEqual((4, 0));
  });

  test("should reset to zero when reaching the loop length", () => {
    let existing = Timing.start
      |> applyMultipleTimes(4, Timing.advance(4, NoSync));

    expect((Timing.index(existing), Timing.subIndex(existing))) |> toEqual((0, 0));
  });

  describe("merge", () => {
    test("should reset timing if the existing index exceeds the loop length", () => {
      let incoming = Timing.start;

      let existing = Timing.start
        |> applyMultipleTimes(4, Timing.advance(8, NoSync));

      let merged = Timing.merge(4, incoming, existing);

      expect((Timing.index(merged), Timing.subIndex(merged))) |> toEqual((0, 0));
    });

    test("should advance if the existing subIndex value is invalid (>= subTicks)", () => {
      // this will have subTicks 1.
      let incoming = Timing.start;

      // set to index 0, subIndex 1, subTicks 2 (>= incoming.subTicks).
      let existing = Timing.start
        |> Timing.setSubTicks(2)
        |> Timing.advance(8, NoSync);

      // should advance existing.
      let merged = Timing.merge(4, incoming, existing);

      expect((Timing.index(merged), Timing.subIndex(merged))) |> toEqual((1, 0));
    });

    test("should keep existing timing otherwise", () => {
      let incoming = Timing.start;

      let existing = Timing.start
        |> applyMultipleTimes(4, Timing.advance(8, NoSync));

      let merged = Timing.merge(8, incoming, existing);

      expect(merged) |> toEqual(existing);
    });
  });
});
