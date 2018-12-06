include Set.Make(Char);

let to_string = (char_set) =>
  switch (cardinal(char_set)) {
  | 0 => "ε"
  | 1 => String.make(1, choose(char_set))
  | 256 => "."
  | _ =>
    "["
    ++ String.concat("", List.map(String.make(1), elements(char_set)))
    ++ "]"
  };

let example = (char_list) => of_list(char_list);

let test = () => {
  assert(to_string(example([])) == "ε");
  assert(to_string(example(['a'])) == "a");
  assert(to_string(example(['b','a','c'])) == "[abc]");
};
