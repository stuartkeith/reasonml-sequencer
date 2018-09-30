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

let value: (t) => array(int);
