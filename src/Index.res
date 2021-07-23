Random.self_init();

switch (ReactDOM.querySelector("#app")){
  | Some(root) => ReactDOM.render(<App />, root)
  | None => () // do nothing
}
