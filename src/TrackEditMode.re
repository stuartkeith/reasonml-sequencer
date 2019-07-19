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
  | ApplyUpdateToExistingValues
  | ApplyUpdateToValues('a)
  | RestoreValues('a)
  | PushUndoValues('a);

let updateEditMode = (id, targetValues, updateIndex, mouseAction, editMode) => {
  switch (editMode, mouseAction) {
    | (Inactive, MouseEnter) => Preview({ id, valuesBeforeEdit: targetValues, index: updateIndex })
    | (Inactive, MouseMove) => editMode
    | (Inactive, MouseLeave) => editMode
    | (Inactive, MouseDown) => editMode
    | (Inactive, MouseUp) => editMode
    | (Preview(_), MouseEnter) => editMode
    | (Preview(preview), MouseMove) when preview.index !== updateIndex => Preview({ id, valuesBeforeEdit: preview.valuesBeforeEdit, index: updateIndex })
    | (Preview(_), MouseMove) => editMode
    | (Preview(_), MouseLeave) => Inactive
    | (Preview(preview), MouseDown) => Active({ id, valuesBeforeEdit: preview.valuesBeforeEdit, mousePosition: Inside })
    | (Preview(_), MouseUp) => editMode
    | (Active(active), MouseEnter) => Active({ id, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Inside })
    | (Active(_), MouseMove) => editMode
    | (Active(active), MouseLeave) => Active({ id, valuesBeforeEdit: active.valuesBeforeEdit, mousePosition: Outside })
    | (Active(_), MouseDown) => editMode
    | (Active(active), MouseUp) => switch (active.mousePosition) {
      | Inside => Preview({ id, valuesBeforeEdit: targetValues, index: updateIndex })
      | Outside => Inactive
    }
  }
};

let getSideEffects = (previousEditMode, newEditMode) => {
  switch (previousEditMode, newEditMode) {
    | (Inactive, Inactive) => NoSideEffects
    | (Inactive, Preview(_)) => ApplyUpdateToExistingValues
    | (Inactive, Active(_)) => NoSideEffects
    | (Preview(preview), Inactive) => RestoreValues(preview.valuesBeforeEdit)
    | (Preview(previousPreview), Preview(preview)) when previousPreview.index !== preview.index => ApplyUpdateToValues(preview.valuesBeforeEdit)
    | (Preview(_), Preview(_)) => ApplyUpdateToExistingValues
    | (Preview(_), Active(_)) => NoSideEffects
    | (Active(active), Inactive) => PushUndoValues(active.valuesBeforeEdit)
    | (Active(_), Preview(_)) => NoSideEffects
    | (Active(_), Active(_)) => ApplyUpdateToExistingValues
  }
};
