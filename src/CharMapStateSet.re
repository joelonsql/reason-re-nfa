module M = Map.Make(Char);

let to_string = (char_map_state_set) =>
  "{" ++ String.concat(
    ",",
    List.map(
      fun ((c,ss)) => String.make(1,c) ++ ":" ++ StateSet.to_string(ss),
      M.bindings(char_map_state_set)
    )
  ) ++ "}";

let example = (char, state_list) =>
  M.singleton(char, StateSet.example(state_list));

let test = () => {
  assert(to_string(example('a',[0])) == "{a:{0}}");
};
