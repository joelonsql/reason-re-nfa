include Set.Make(Char);

let to_string = char_set =>
  switch (cardinal(char_set)) {
  | 0 => "ε"
  | 1 => Common.escaped_double_quote(choose(char_set))
  | 256 => "."
  | _ =>
    "["
    ++ String.concat(
         "",
         List.map(Common.escaped_double_quote, elements(char_set)),
       )
    ++ "]"
  };

let example = char_list => of_list(char_list);

let test = () => {
  assert(to_string(example([])) == "ε");
  assert(to_string(example(['a'])) == "a");
  assert(to_string(example(['"'])) == "\\\"");
  assert(to_string(example([Char.chr(1)])) == "\\\\x01");
  assert(to_string(example(['b', 'a', 'c'])) == "[abc]");
};