open Jest;
open Expect;

describe("TrackEditMode", () => {
  test("preview should handle mouse movements between indices", () => {
    open TrackEditMode;

    let id = Id.create();

    let result = updateEditMode(id, [|1|], 1, MouseMove, Preview({
      id,
      valuesBeforeEdit: [|2|],
      index: 0
    }));

    expect(result) |> toEqual((
      Preview({
        id,
        valuesBeforeEdit: [|2|],
        index: 1
      }),
      ApplyUpdateToValues(id, [|2|])
    ));
  });

  test("preview should handle mouse movements within one index", () => {
    open TrackEditMode;

    let id = Id.create();

    let result = updateEditMode(id, [|1|], 0, MouseMove, Preview({
      id,
      valuesBeforeEdit: [|2|],
      index: 0
    }));

    expect(result) |> toEqual((
      Preview({
        id,
        valuesBeforeEdit: [|2|],
        index: 0
      }),
      ApplyUpdateToValues(id, [|1|])
    ));
  });

  let nonActiveTest = (label, editMode) => {
    open TrackEditMode;

    [
      ("mouse enter", MouseEnter),
      ("mouse move", MouseMove),
      ("mouse leave", MouseLeave),
      ("mouse down", MouseDown),
      ("mouse up", MouseUp)
    ] |> List.iter(((mouseEventName, mouseEvent)) => {
      test(label ++ " should ignore " ++ mouseEventName ++ " from non-active component", () => {
        let inactiveId = Id.create();

        let result = updateEditMode(inactiveId, [|1|], 0, mouseEvent, editMode);

        expect(result) |> toEqual((
          editMode,
          NoSideEffects
        ));
      });
    });
  };

  nonActiveTest("preview", Preview({
    id: Id.create(),
    valuesBeforeEdit: [|2|],
    index: 0
  }));

  nonActiveTest("active", Active({
    id: Id.create(),
    valuesBeforeEdit: [|2|],
    mousePosition: Outside
  }));
});
