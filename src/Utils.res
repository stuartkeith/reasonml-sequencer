let rec getOffset = (element, x, y) => {
  switch (element) {
    | None => (x, y)
    | Some(element) => {
      open Webapi.Dom;

      switch (Element.asHtmlElement(element)) {
        | Some(htmlElement) => getOffset(HtmlElement.offsetParent(htmlElement), x - HtmlElement.offsetLeft(htmlElement), y - HtmlElement.offsetTop(htmlElement))
        | None => (x, y)
      }
    }
  }
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
