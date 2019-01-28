include Map.Make(RangeSet);

exception NoElement(string);
exception MultipleElements(string);

let union = (type a, f: (RangeSet.t, a, a) => option(a)) => {
  let f = (k, x, y) =>
    switch (x, y) {
    | (None, None) => None
    | (Some(v), None) => Some(v)
    | (None, Some(v)) => Some(v)
    | (Some(v1), Some(v2)) => f(k, v1, v2)
    };

  merge(f);
};

let choose_strict = ranges_map =>
  switch (cardinal(ranges_map)) {
  | 1 => choose(ranges_map)
  | 0 => raise(NoElement("Expected 1 element, got 0"))
  | n =>
    raise(MultipleElements("Expected 1 element, got " ++ string_of_int(n)))
  };