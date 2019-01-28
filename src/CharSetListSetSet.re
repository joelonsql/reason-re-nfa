include Set.Make(CharSetListSet);

let to_string = string_set_set =>
  "{"
  ++ String.concat(
       " ",
       List.map(CharSetListSet.to_string, elements(string_set_set)),
     )
  ++ "}";