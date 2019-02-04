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

let merge = (s1, s2) => {
  switch (cardinal(s1), cardinal(s2)) {
  | (_, 0) => s1
  | (0, _) => s2
  | (_, _) =>
    fold(
      (l1, s3) => fold((l2, s3) => add(l1 @ l2, s3), s2, s3),
      s1,
      empty,
    )
  };
};

let test = () => {
  assert(
    to_string(singleton([RangeSet.singleton(Range.singleton('a', 'a'))]))
    == "a",
  );
  assert(
    to_string(singleton([RangeSet.singleton(Range.singleton('a', 'c'))]))
    == "[a-c]",
  );
  let ac02 =
    singleton([
      RangeSet.singleton(Range.singleton('a', 'c')),
      RangeSet.singleton(Range.singleton('0', '2')),
    ]);
  let df34 =
    singleton([
      RangeSet.singleton(Range.singleton('d', 'f')),
      RangeSet.singleton(Range.singleton('3', '4')),
    ]);
  let gi56 =
    singleton([
      RangeSet.singleton(Range.singleton('g', 'i')),
      RangeSet.singleton(Range.singleton('5', '6')),
    ]);
  let jl78 =
    singleton([
      RangeSet.singleton(Range.singleton('j', 'l')),
      RangeSet.singleton(Range.singleton('7', '8')),
    ]);
  let ac02df34 = union(ac02, df34);
  let gi56jl78 = union(gi56, jl78);
  let m = merge(ac02df34, gi56jl78);
  assert(to_string(ac02df34) == "([a-c][0-2]|[d-f][3-4])");
  assert(to_string(gi56jl78) == "([g-i][5-6]|[j-l][7-8])");
  assert(
    to_string(m)
    == "([a-c][0-2][g-i][5-6]|[a-c][0-2][j-l][7-8]|[d-f][3-4][g-i][5-6]|[d-f][3-4][j-l][7-8])",
  );
};