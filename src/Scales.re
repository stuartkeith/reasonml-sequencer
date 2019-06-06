type t = array(int);

type steps =
  | Whole
  | Half;

let majorScale = [|Whole, Whole, Half, Whole, Whole, Whole, Half|];

let generateScale = (majorScaleOffset) => {
  let result = Array.make(Array.length(majorScale), 0);

  for (i in 1 to Array.length(result) - 1) {
    let value = majorScale[(i - 1 + majorScaleOffset) mod Array.length(majorScale)];

    let increment = switch (value) {
      | Whole => 2
      | Half => 1
    };

    result[i] = result[i - 1] + increment;
  };

  result;
};

let scaleChromatic = [|0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11|];
let scaleMajor = generateScale(0);
let scaleDorian = generateScale(1);
let scalePhrygian = generateScale(2);
let scaleLydian = generateScale(3);
let scaleMixolydian = generateScale(4);
let scaleMinor = generateScale(5);
let scaleLocrian = generateScale(6);

let scales = [|
  ("Chromatic", scaleChromatic),
  ("Major", scaleMajor),
  ("Dorian", scaleDorian),
  ("Phrygian", scalePhrygian),
  ("Lydian", scaleLydian),
  ("Mixolydian", scaleMixolydian),
  ("Minor", scaleMinor),
  ("Locrian", scaleLocrian)
|];
