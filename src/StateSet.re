module S = Set.Make(Int32);

let to_string = (state_set) =>
  "{" ++ String.concat(
    " ",
    List.map(Int32.to_string, S.elements(state_set)
    )
  ) ++ "}";

let example = (state_list) => List.map(Int32.of_int, state_list)->S.of_list;

let test = () => {
  assert(to_string(example([])) == "{}");
  assert(to_string(example([0])) == "{0}");
  assert(to_string(example([1,0,2])) == "{0 1 2}");
};
