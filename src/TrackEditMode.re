type mouseAction =
  | MouseEnter
  | MouseMove
  | MouseLeave
  | MouseDown
  | MouseUp;

type preview('a) = {
  id: Id.t,
  valuesBeforeEdit: 'a,
  index: int
};

type mousePosition('a) =
  | Inside
  | Outside
  | InsideAnother(Id.t, 'a, SynthValues.update);

type active('a) = {
  id: Id.t,
  valuesBeforeEdit: 'a,
  mousePosition: mousePosition('a)
};

type editMode('a) =
  | Inactive
  | Preview(preview('a))
  | Active(active('a));

type sideEffects('a) =
  | NoSideEffects
  | ApplyUpdateToValues(Id.t, 'a, SynthValues.update)
  | RestoreValues(Id.t, 'a);

let updateEditMode = (targetId, targetValues, update: SynthValues.update, mouseAction, editMode) => {
  let noOp = (editMode, NoSideEffects, None);

  switch (editMode, mouseAction) {
    | (Inactive, MouseEnter) => (
      Preview({ id: targetId, valuesBeforeEdit: targetValues, index: update.index }),
      ApplyUpdateToValues(targetId, targetValues, update),
      None
    )
    | (Inactive, MouseMove) => noOp
    | (Inactive, MouseLeave) => noOp
    | (Inactive, MouseDown) => noOp
    | (Inactive, MouseUp) => noOp
    | (Preview(preview), _) when preview.id !== targetId => noOp
    | (Preview(_), MouseEnter) => noOp
    | (Preview(preview), MouseMove) when preview.index !== update.index => (
      Preview({ id: preview.id, valuesBeforeEdit: preview.valuesBeforeEdit, index: update.index }),
      ApplyUpdateToValues(preview.id, preview.valuesBeforeEdit, update),
      None
    )
    | (Preview(preview), MouseMove) => (
      editMode,
      ApplyUpdateToValues(preview.id, targetValues, update),
      None
    )
    | (Preview(preview), MouseLeave) => (
      Inactive,
      RestoreValues(preview.id, preview.valuesBeforeEdit),
      None
    )
    | (Preview(preview), MouseDown) => (
      Active({ id: preview.id, valuesBeforeEdit: preview.valuesBeforeEdit, mousePosition: Inside }),
      NoSideEffects,
      None
    )
    | (Preview(_), MouseUp) => noOp
    | (Active(active), MouseEnter)
    | (Active(active), MouseMove) when active.id !== targetId => (
      Active({ ...active, mousePosition: InsideAnother(targetId, targetValues, update) }),
      NoSideEffects,
      None
    )
    | (Active(active), MouseLeave) when active.id !== targetId => (
      Active({ ...active, mousePosition: Outside }),
      NoSideEffects,
      None
    )
    | (Active(active), _) when active.id !== targetId => noOp
    | (Active(active), MouseEnter) => (
      Active({ id: active.id, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Inside }),
      NoSideEffects,
      None
    )
    | (Active(active), MouseMove) => (
      editMode,
      ApplyUpdateToValues(active.id, targetValues, update),
      None
    )
    | (Active(active), MouseLeave) when active.id === targetId => (
      Active({ id: targetId, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Outside }),
      NoSideEffects,
      None
    )
    | (Active(_), MouseLeave) => noOp
    | (Active(active), MouseDown) => (
      editMode,
      ApplyUpdateToValues(active.id, targetValues, update),
      None
    )
    | (Active(active), MouseUp) => switch (active.mousePosition) {
      | Inside => (
        Preview({ id: active.id, valuesBeforeEdit: targetValues, index: update.index }),
        NoSideEffects,
        Some((active.id, active.valuesBeforeEdit))
      )
      | Outside => (
        Inactive,
        NoSideEffects,
        Some((active.id, active.valuesBeforeEdit))
      )
      | InsideAnother(id, values, update) => (
        Preview({ id, valuesBeforeEdit: values, index: update.index }),
        ApplyUpdateToValues(id, values, update),
        Some((active.id, active.valuesBeforeEdit))
      )
    }
  }
};
