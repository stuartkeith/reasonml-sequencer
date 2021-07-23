let getOffset = (element) => {
  let rec traverse = (element, x, y) => {
    switch (element) {
      | Some(obj) => traverse(Js.Nullable.toOption(obj["offsetParent"]), x - obj["offsetLeft"], y - obj["offsetTop"])
      | None => (x, y)
    }
  };

  let elementObject = Belt.Option.map(element, ReactDOM.domElementToObj)

  traverse(elementObject, 0, 0)
};

let limit = (value, min, max) => Pervasives.min(max, Pervasives.max(min, value));

let randomArrayValue = (array) => array[Random.int(Array.length(array))];

let randomInt = (min, max) => min + Random.int(max - min + 1);

let randomFloat = (min, max) => min +. Random.float(max -. min);

let rec getArrayIndex = (array, value, index) => {
  if (array[index] === value) {
    index;
  } else {
    getArrayIndex(array, value, index + 1);
  }
};
