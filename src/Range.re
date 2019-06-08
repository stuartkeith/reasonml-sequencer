[@react.component]
let make = (~label:string, ~value:float, ~min:float, ~max:float, ~step:float, ~onChange) => {
  let scale = 1.0 -. ((value -. min) /. (max -. min));

  let style = ReactDOMRe.Style.make(
    ~transformOrigin="0 100%",
    ~transform=Printf.sprintf("translate3d(%g%%, 0, 0)", scale *. -100.0),
    ()
  );

  <div className="relative w4 h2 flex-none">
    <div className="relative bg-light-gray dark-gray overflow-hidden h-100">
      <div className="absolute absolute--fill bg-gray" style />
    </div>
    <div className="absolute absolute--fill flex items-center justify-center">
      (React.string(label))
    </div>
    <input
      type_="range"
      className="input-range-reset pointer absolute absolute--fill w-100"
      value=Js.Float.toString(value)
      min=Js.Math.floor(min)
      max=Js.Float.toString(max)
      step
      onChange=((event) => {
        let value = event->ReactEvent.Form.target##value;

        onChange(value);
      })
    />
  </div>
};
