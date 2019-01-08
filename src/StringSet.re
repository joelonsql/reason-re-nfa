include Set.Make(String);

let to_string = string_set =>
  switch (cardinal(string_set)) {
  | 0 => "ε"
  | 1 => choose(string_set)
  | _ => "(" ++ String.concat("|", elements(string_set)) ++ ")"
  };

let example = char_list => of_list(char_list);

let test = () => {
  assert(to_string(example([])) == "ε");
  assert(to_string(example(["a"])) == "a");
  assert(to_string(example(["b", "a", "c"])) == "(a|b|c)");
  assert(to_string(example(["def", "abc", "def"])) == "(abc|def)");
};
