type t = Chromatic | Major | Dorian | Phrygian | Lydian | Mixolydian | Minor | Locrian;

type steps =
  | Whole
  | Half;

let majorScale = [|Whole, Whole, Half, Whole, Whole, Whole, Half|];

let generateTranspose = (majorScaleOffset) => {
  let result = Array.make(Array.length(majorScale) + 1, 0);

  for (i in 1 to Array.length(majorScale) - 1) {
    let value = majorScale[(i + majorScaleOffset) mod Array.length(majorScale)];

    let increment = switch (value) {
      | Whole => 2
      | Half => 1
    };

    result[i + 1] = result[i] + increment;
  };

  result;
};

let transposeChromatic = [|0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11|];
let transposeMajor = generateTranspose(0);
let transposeDorian = generateTranspose(1);
let transposePhrygian = generateTranspose(2);
let transposeLydian = generateTranspose(3);
let transposeMixolydian = generateTranspose(4);
let transposeMinor = generateTranspose(5);
let transposeLocrian = generateTranspose(6);

let getScale = (t) => switch (t) {
  | Chromatic => transposeChromatic
  | Major => transposeMajor
  | Dorian => transposeDorian
  | Phrygian => transposePhrygian
  | Lydian => transposeLydian
  | Mixolydian => transposeMixolydian
  | Minor => transposeMinor
  | Locrian => transposeLocrian
};

let max = (t) => {
  let array = getScale(t);

  Array.length(array) - 1;
}

let value = (note, t) => {
  let array = getScale(t);

  array[note mod Array.length(array)];
};
