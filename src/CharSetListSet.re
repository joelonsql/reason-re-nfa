include Set.Make(CharSetList);

let to_string = string_set =>
  switch (cardinal(string_set)) {
  | 0 => ""
  | 1 => CharSetList.to_string(choose(string_set))
  | _ =>
    "("
    ++ String.concat(
         "|",
         List.map(CharSetList.to_string, elements(string_set)),
       )
    ++ ")"
  };

let test = () => {
  assert(to_string(singleton([CharSet.singleton('a')])) == "a");
  assert(
    to_string(singleton([CharSet.singleton('a'), CharSet.singleton('b')]))
    == "(a|b)",
  );
  assert(
    to_string(
      singleton([
        CharSet.singleton('a'),
        CharSet.add('b', CharSet.singleton('c')),
        CharSet.singleton('d'),
      ]),
    )
    == "(a|[b-c]|d)",
  );
};