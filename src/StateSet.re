include Set.Make(Int32);

exception NoElement(string);
exception MultipleElements(string);

let to_string = state_set =>
  switch (cardinal(state_set)) {
  | 1 => Int32.to_string(choose(state_set))
  | _ =>
    "{"
    ++ String.concat(" ", List.map(Int32.to_string, elements(state_set)))
    ++ "}"
  };

let to_identifier = state_set =>
  "state"
  ++ String.concat("_", List.map(Int32.to_string, elements(state_set)));

let of_ints = state_list => List.map(Int32.of_int, state_list) |> of_list;

let test = () => {
  assert(to_string(of_ints([])) == "{}");
  assert(to_string(of_ints([0])) == "0");
  assert(to_string(of_ints([1, 0, 2])) == "{0 1 2}");
  assert(to_identifier(of_ints([1, 0, 2])) == "state_0_1_2");
};

let choose_strict: t => int32 =
  state_set =>
    switch (cardinal(state_set)) {
    | 1 => choose(state_set)
    | 0 => raise(NoElement("Expected 1 element, got 0"))
    | n =>
      raise(
        MultipleElements(
          "Expected 1 element, got "
          ++ string_of_int(n)
          ++ " : "
          ++ to_string(state_set),
        ),
      )
    };