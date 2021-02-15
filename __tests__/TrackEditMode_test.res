open Jest;
open Expect;

describe("TrackEditMode", () => {
  test("preview should handle mouse movements between indices", () => {
    open TrackEditMode;

    let id = Id.create();

    let result = updateEditMode(id, [1], { index: 1, value: 0.0 }, MouseMove, Preview({
      id,
      valuesBeforeEdit: [2],
      index: 0
    }));

    expect(result) |> toEqual((
      Preview({
        id,
        valuesBeforeEdit: [2],
        index: 1
      }),
      ApplyUpdateToValues(id, [2], { index: 1, value: 0.0 }),
      None
    ));
  });

  test("preview should handle mouse movements within one index", () => {
    open TrackEditMode;

    let id = Id.create();

    let result = updateEditMode(id, [1], { index: 0, value: 0.0 }, MouseMove, Preview({
      id,
      valuesBeforeEdit: [2],
      index: 0
    }));

    expect(result) |> toEqual((
      Preview({
        id,
        valuesBeforeEdit: [2],
        index: 0
      }),
      ApplyUpdateToValues(id, [1], { index: 0, value: 0.0 }),
      None
    ));
  });

  test("active should queue up the next preview for a non-active component that's mouse entered", () => {
    open TrackEditMode;

    let previewId = Id.create();
    let id = Id.create();

    let result = updateEditMode(previewId, [1], { index: 0, value: 0.0 }, MouseMove, Active({
      id,
      valuesBeforeEdit: [2],
      mousePosition: Outside
    }));

    expect(result) |> toEqual((
      Active({
        id,
        valuesBeforeEdit: [2],
        mousePosition: InsideAnother(
          previewId,
          [1],
          { index: 0, value: 0.0 }
        )
      }),
      NoSideEffects,
      None
    ));
  });

  test("active should switch to the preview of a non-active component", () => {
    open TrackEditMode;

    let previewId = Id.create();
    let id = Id.create();

    let result = updateEditMode(id, [1], { index: 0, value: 0.0 }, MouseUp, Active({
      id,
      valuesBeforeEdit: [2],
      mousePosition: InsideAnother(
        previewId,
        [3],
        { index: 4, value: 123.0 }
      )
    }));

    expect(result) |> toEqual((
      Preview({
        id: previewId,
        valuesBeforeEdit: [3],
        index: 4
      }),
      ApplyUpdateToValues(previewId, [3], { index: 4, value: 123.0 }),
      Some((id, [2]))
    ));
  });

  let nonActiveTest = (label, editMode, events) => {
    open TrackEditMode;

    Array.iter(((mouseEventName, mouseEvent)) => {
      test(label ++ " should ignore " ++ mouseEventName ++ " from non-active component", () => {
        let inactiveId = Id.create();

        let result = updateEditMode(inactiveId, [1], { index: 0, value: 0.0 }, mouseEvent, editMode);

        expect(result) |> toEqual((
          editMode,
          NoSideEffects,
          None
        ));
      });
    }, events);
  };

  [
    ("mouse enter", TrackEditMode.MouseEnter),
    ("mouse move", TrackEditMode.MouseMove),
    ("mouse leave", TrackEditMode.MouseLeave),
    ("mouse down", TrackEditMode.MouseDown),
    ("mouse up", TrackEditMode.MouseUp)
  ] |>
    nonActiveTest("preview", Preview({
      id: Id.create(),
      valuesBeforeEdit: [2],
      index: 0
    }));

  [
    ("mouse down", TrackEditMode.MouseDown),
    ("mouse up", TrackEditMode.MouseUp)
  ] |>
    nonActiveTest("active", Active({
      id: Id.create(),
      valuesBeforeEdit: [2],
      mousePosition: Outside
    }));
});
