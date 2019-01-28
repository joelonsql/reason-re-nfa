include Set.Make(RangeSetList);

let to_string = range_set_list =>
  switch (cardinal(range_set_list)) {
  | 0 => ""
  | 1 => RangeSetList.to_string(choose(range_set_list))
  | _ =>
    "("
    ++ String.concat(
         "|",
         List.map(RangeSetList.to_string, elements(range_set_list)),
       )
    ++ ")"
  };

let test = () => {
  assert(
    to_string(singleton([RangeSet.singleton(Range.singleton('a', 'a'))]))
    == "[a]",
  );
  assert(
    to_string(singleton([RangeSet.singleton(Range.singleton('a', 'c'))]))
    == "[a-c]",
  );
  assert(
    to_string(
      union(
        singleton([
          RangeSet.singleton(Range.singleton('a', 'c')),
          RangeSet.singleton(Range.singleton('0', '2')),
        ]),
        singleton([
          RangeSet.singleton(Range.singleton('d', 'f')),
          RangeSet.singleton(Range.singleton('3', '4')),
        ]),
      ),
    )
    == "([a-c][0-2]|[d-f][3-4])",
  );
};