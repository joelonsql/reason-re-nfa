include Set.Make(RangeSetListSet);

let to_string = ranges_set =>
  "{"
  ++ String.concat(
       " ",
       List.map(RangeSetListSet.to_string, elements(ranges_set)),
     )
  ++ "}";