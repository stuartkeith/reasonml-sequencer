@react.component
let make = (~value:float, ~min:float, ~max:float, ~step:float, ~onChange, ~children) => {
  let scale = 1.0 -. ((value -. min) /. (max -. min));
  let scaleString = Belt.Float.toString(scale *. -100.0);

  let style = ReactDOM.Style.make(
    ~transformOrigin="0 100%",
    ~transform=`translate3d(${scaleString}%, 0, 0)`,
    ()
  );

  <div className="relative w4 h2 flex-none">
    <div className="relative bg-light-gray dark-gray overflow-hidden h-100">
      <div className="absolute absolute--fill bg-gray" style />
    </div>
    <div className="absolute absolute--fill flex items-center justify-center">
      (children)
    </div>
    <input
      type_="range"
      className="input-range-reset pointer absolute absolute--fill w-100"
      value=Js.Float.toString(value)
      min=Js.Float.toString(min)
      max=Js.Float.toString(max)
      step
      onChange=((event) => {
        let value = ReactEvent.Form.target(event)["value"];

        onChange(value);
      })
    />
  </div>
};
