include Map.Make(RangeSetListSet);

exception NoElement(string);
exception MultipleElements(string);

let union =
    (f: (RangeSetListSet.t, StateSet.t, StateSet.t) => option(StateSet.t)) => {
  let f = (k, x, y) =>
    switch (x, y) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(v1), Some(v2)) => f(k, v1, v2)
    };

  merge(f);
};

let count_strings = range_set_list_set_map =>
  fold(
    (range_set_list_set, _, c) =>
      RangeSetListSet.count_strings(range_set_list_set) + c,
    range_set_list_set_map,
    0,
  );

let choose_strict = ranges_map =>
  switch (cardinal(ranges_map)) {
  | 1 => choose(ranges_map)
  | 0 => raise(NoElement("Expected 1 element, got 0"))
  | n =>
    raise(MultipleElements("Expected 1 element, got " ++ string_of_int(n)))
  };