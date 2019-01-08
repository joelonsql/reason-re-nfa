include Set.Make(StringSet);

let to_string = string_set_set =>
  "{"
  ++ String.concat(
       " ",
       List.map(StringSet.to_string, elements(string_set_set)),
     )
  ++ "}";
