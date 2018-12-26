include Set.Make(CharSet);

let to_string = (char_set_set) =>
  "{"
  ++ String.concat(" ", List.map(CharSet.to_string, elements(char_set_set)))
  ++ "}";
