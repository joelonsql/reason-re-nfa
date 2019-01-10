include Set.Make(StateSet);

exception NoElement(string);
exception MultipleElements(string);

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

let choose_strict: t => StateSet.t =
  state_set_set =>
    switch (cardinal(state_set_set)) {
    | 1 => choose(state_set_set)
    | 0 => raise(NoElement("Expected 1 element, got 0"))
    | n =>
      raise(
        MultipleElements("Expected 1 element, got " ++ string_of_int(n)),
      )
    };