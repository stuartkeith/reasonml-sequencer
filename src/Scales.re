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

let transposeMajor = generateTranspose(0);
let transposeDorian = generateTranspose(1);
let transposePhrygian = generateTranspose(2);
let transposeLydian = generateTranspose(3);
let transposeMixolydian = generateTranspose(4);
let transposeMinor = generateTranspose(5);
let transposeLocrian = generateTranspose(6);
