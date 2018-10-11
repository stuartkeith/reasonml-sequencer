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

let jsFloor: (float) => string = [%bs.raw {|
  function (x) { return Math.floor(x).toString(); }
|}];

let jsRound: (float) => int = [%bs.raw {|
  function (x) { return Math.round(x); }
|}];

let jsFloatToString: (float) => string = [%bs.raw {|
  function (x) { return x.toString(); }
|}];

let limit = (value, min, max) => Pervasives.min(max, Pervasives.max(min, value));
