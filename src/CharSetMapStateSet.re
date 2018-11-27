module M = Map.Make(CharSet.S);

let to_string = (char_set_map_state_set) =>
  "{" ++ String.concat(
    ",",
    List.rev(M.fold(
      fun (char_set,state_set,l) => [CharSet.to_string(char_set) ++ ":" ++ StateSet.to_string(state_set), ...l],
      char_set_map_state_set,
      []
    ))
  ) ++ "}";

let example = (char_list, state_list) =>
  M.singleton(
    CharSet.example(char_list),
    StateSet.example(state_list)
  );

let test = () => {
    assert(to_string(example(['a','b'], [0,1])) == "{{a b}:{0 1}}");
};
