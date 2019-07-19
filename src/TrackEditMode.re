type mouseAction =
  | MouseEnter
  | MouseMove
  | MouseLeave
  | MouseDown
  | MouseUp;

type mousePosition =
  | Inside
  | Outside;

type preview('a) = {
  id: Id.t,
  valuesBeforeEdit: 'a,
  index: int
};

type active('a) = {
  id: Id.t,
  valuesBeforeEdit: 'a,
  mousePosition
};

type editMode('a) =
  | Inactive
  | Preview(preview('a))
  | Active(active('a));

type sideEffects('a) =
  | NoSideEffects
  | ApplyUpdateToValues(Id.t, 'a)
  | RestoreValues(Id.t, 'a)
  | PushUndoValues(Id.t, 'a);

let updateEditMode = (targetId, targetValues, updateIndex, mouseAction, editMode) => {
  let noOp = (editMode, NoSideEffects);

  switch (editMode, mouseAction) {
    | (Inactive, MouseEnter) => (
      Preview({ id: targetId, valuesBeforeEdit: targetValues, index: updateIndex }),
      ApplyUpdateToValues(targetId, targetValues)
    )
    | (Inactive, MouseMove) => noOp
    | (Inactive, MouseLeave) => noOp
    | (Inactive, MouseDown) => noOp
    | (Inactive, MouseUp) => noOp
    | (Preview(preview), _) when preview.id !== targetId => noOp
    | (Preview(_), MouseEnter) => noOp
    | (Preview(preview), MouseMove) when preview.index !== updateIndex => (
      Preview({ id: preview.id, valuesBeforeEdit: preview.valuesBeforeEdit, index: updateIndex }),
      ApplyUpdateToValues(preview.id, preview.valuesBeforeEdit)
    )
    | (Preview(preview), MouseMove) => (
      editMode,
      ApplyUpdateToValues(preview.id, targetValues)
    )
    | (Preview(preview), MouseLeave) => (
      Inactive,
      RestoreValues(preview.id, preview.valuesBeforeEdit)
    )
    | (Preview(preview), MouseDown) => (
      Active({ id: preview.id, valuesBeforeEdit: preview.valuesBeforeEdit, mousePosition: Inside }),
      NoSideEffects
    )
    | (Preview(_), MouseUp) => noOp
    | (Active(active), _) when active.id !== targetId => noOp
    | (Active(active), MouseEnter) => (
      Active({ id: active.id, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Inside }),
      NoSideEffects
    )
    | (Active(active), MouseMove) => (
      editMode,
      ApplyUpdateToValues(active.id, targetValues)
    )
    | (Active(active), MouseLeave) when active.id === targetId => (
      Active({ id: targetId, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Outside }),
      NoSideEffects
    )
    | (Active(_), MouseLeave) => noOp
    | (Active(active), MouseDown) => (
      editMode,
      ApplyUpdateToValues(active.id, targetValues)
    )
    | (Active(active), MouseUp) => switch (active.mousePosition) {
      | Inside => (
        Preview({ id: active.id, valuesBeforeEdit: targetValues, index: updateIndex }),
        PushUndoValues(active.id, active.valuesBeforeEdit)
      )
      | Outside => (
        Inactive,
        PushUndoValues(active.id, active.valuesBeforeEdit)
      )
    }
  }
};
