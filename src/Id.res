type t = string;

let nextId = ref(0);

let create = () => {
  nextId := nextId.contents + 1;

  string_of_int(nextId.contents);
};

let equals = (idOne, idTwo) => idOne === idTwo;

let toString = (id) => id;
