type t =
  | Solo
  | Major
  | Minor
  | Diminished
  | MajorSeventh
  | MinorSeventh
  | DominantSeventh
  | Suspended2
  | Suspened4
  | Augmented;

let solo = [||];
let major = [|4, 7|];
let minor = [|3, 7|];
let diminished = [|3, 6|];
let majorSeventh = [|4, 7, 11|];
let minorSeventh = [|3, 7, 10|];
let dominantSeventh = [|4, 7, 10|];
let suspended2 = [|2, 7|];
let suspended4 = [|5, 7|];
let augmented = [|4, 8|];

let value = (t) => switch (t) {
  | Solo => solo
  | Major => major
  | Minor => minor
  | Diminished => diminished
  | MajorSeventh => majorSeventh
  | MinorSeventh => minorSeventh
  | DominantSeventh => dominantSeventh
  | Suspended2 => suspended2
  | Suspened4 => suspended4
  | Augmented => augmented
};
