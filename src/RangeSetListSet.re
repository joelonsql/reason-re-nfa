include Set.Make(RangeSetList);

exception NoElement(string);
exception MultipleElements(string);

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

let choose_strict: t => RangeSet.t =
  r =>
    switch (cardinal(r)) {
    | 1 => RangeSetList.choose_strict(choose(r))
    | 0 => raise(NoElement("Expected 1 element, got 0"))
    | n =>
      raise(
        MultipleElements("Expected 1 element, got " ++ string_of_int(n)),
      )
    };

let of_char: char => t =
  c => {
    singleton([RangeSet.of_char(c)]);
  };

let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [of_char(s.[i]), ...l]);
    };
  exp(String.length(s) - 1, []);
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