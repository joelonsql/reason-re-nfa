module S = Set.Make(Int32);

let to_string = (s) => "{" ++
  String.concat(
    " ",
    List.map(
      Int32.to_string,
      S.elements(s)
    )
  ) ++ "}";

let example = (state_list) => List.fold_left(
  (state_set, state) => S.add(
    Int32.of_int(state),
    state_set
  ),
  S.empty,
  state_list
);

let test = () => {
  assert(to_string(example([])) == "{}");
  assert(to_string(example([0])) == "{0}");
  assert(to_string(example([1,0,2])) == "{0 1 2}");
};
