include Map.Make(Char);

let to_string = (char_map_state) =>
  "{" ++ String.concat(
    ",",
    List.map(
      fun ((c,s)) => String.make(1,c) ++ ":" ++ Int32.to_string(s),
      bindings(char_map_state)
    )
  ) ++ "}";

let example = (char, state) =>
  singleton(char, Int32.of_int(state));

let test = () => {
  assert(to_string(example('a',0)) == "{a:0}");
};
