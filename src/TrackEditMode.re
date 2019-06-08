type mousePosition =
  | Inside
  | Outside;

type editMode =
  | Inactive
  | Preview(Id.t, SynthValues.values, int)
  | Active(Id.t, SynthValues.values, mousePosition);
