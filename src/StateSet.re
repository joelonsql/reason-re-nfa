include Set.Make(Int32);

let to_string = state_set =>
  "{"
  ++ String.concat(" ", List.map(Int32.to_string, elements(state_set)))
  ++ "}";

let to_identifier = state_set =>
  String.concat("_", List.map(Int32.to_string, elements(state_set)));

let of_ints = state_list => List.map(Int32.of_int, state_list) |> of_list;

let test = () => {
  assert(to_string(of_ints([])) == "{}");
  assert(to_string(of_ints([0])) == "{0}");
  assert(to_string(of_ints([1, 0, 2])) == "{0 1 2}");
  assert(to_identifier(of_ints([1, 0, 2])) == "0_1_2");
};