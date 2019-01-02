include Set.Make(StateSet);

let to_string = state_set =>
  "{"
  ++ String.concat(" ", List.map(StateSet.to_string, elements(state_set)))
  ++ "}";

let of_ints = state_list => List.map(StateSet.of_ints, state_list) |> of_list;

let test = () => {
  assert(to_string(of_ints([[]])) == "{{}}");
  assert(to_string(of_ints([[0]])) == "{{0}}");
  assert(
    to_string(of_ints([[0, 1], [2, 3], [1, 0]])) == "{{0 1} {2 3}}",
  );
};