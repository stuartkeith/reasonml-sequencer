open Jest;
open Expect;

describe("SynthInstance", () => {
  test("should throw an exception if values length is zero", () => {
    expect(() => {
      SynthInstance.create(SynthValues.emptyValues);
    }) |> toThrow;
  });

  let valueConverter = SynthValues.{
    fromFloat: int_of_float,
    toFloat: float_of_int
  };

  let values = SynthValues.fromArray(valueConverter, [|0, 1, 2, 3|]);
  let synthInstance = SynthInstance.create(values);

  test("should throw an exception if loop length is zero", () => {
    expect(() => {
      SynthInstance.setLoopLength(0, synthInstance);
    }) |> toThrow;
  });

  test("should throw an exception if loop length > the values length", () => {
    expect(() => {
      SynthInstance.setLoopLength(16, synthInstance);
    }) |> toThrow;
  });
});
