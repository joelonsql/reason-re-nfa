include Set.Make(RangeSetSet);

let to_string = range_set_set =>
  "{"
  ++ String.concat(
       " ",
       List.map(RangeSetSet.to_string, elements(range_set_set)),
     )
  ++ "}";