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
