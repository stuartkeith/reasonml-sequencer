[@bs.get] [@bs.return nullable] external offsetParent : HtmlElementRe.t => option(Dom.element) = "";

let rec getOffset = (element, x, y) => {
  switch (element) {
    | None => (x, y)
    | Some(element) => {
      open Webapi.Dom;

      switch (Element.asHtmlElement(element)) {
        | Some(htmlElement) => getOffset(offsetParent(htmlElement), x - HtmlElement.offsetLeft(htmlElement), y - HtmlElement.offsetTop(htmlElement))
        | None => (x, y)
      }
    }
  }
};
