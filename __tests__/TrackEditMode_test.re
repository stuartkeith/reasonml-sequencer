open Jest;
open Expect;

describe("TrackEditMode getSideEffects", () => {
  test("should handle active movements within a component", () => {
    open TrackEditMode;

    let id = Id.create();

    let previousEditMode = Active({
      id,
      valuesBeforeEdit: [|1|],
      mousePosition: Inside
    });

    let nextEditMode = Active({
      id,
      valuesBeforeEdit: [|2|],
      mousePosition: Inside
    });

    let result = getSideEffects(previousEditMode, nextEditMode);

    expect(result) |> toEqual(ApplyUpdateToExistingValues);
  });

  test("should revert values when leaving a component", () => {
    open TrackEditMode;

    let id = Id.create();

    let valuesBeforeEdit = [|1|];

    let previousEditMode = Preview({
      id,
      valuesBeforeEdit,
      index: 0
    });

    let nextEditMode = Inactive;

    let result = getSideEffects(previousEditMode, nextEditMode);

    expect(result) |> toEqual(RestoreValues(valuesBeforeEdit));
  });
});
