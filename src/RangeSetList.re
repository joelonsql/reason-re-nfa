type t = list(RangeSet.t);

exception NoElement(string);
exception MultipleElements(string);
exception DifferentLengths(string);

let to_string: t => string =
  range_set_list => {
    String.concat("", List.map(RangeSet.to_string, range_set_list));
  };

let generate_strings: t => list(string) =
  range_set_list => {
    List.fold_right(
      (range_set, strings) =>
        List.fold_right(
          (string, strings) =>
            List.fold_right(
              (char, strings) =>
                [string ++ String.make(1, char), ...strings],
              RangeSet.to_char_list(range_set),
              strings,
            ),
          strings,
          [],
        ),
      List.rev(range_set_list),
      [""],
    );
  };

let count_strings: t => int =
  range_set_list => {
    List.fold_right(
      (range_set, c) => RangeSet.count_chars(range_set) * c,
      range_set_list,
      1,
    );
  };

let compare = (l, r) => {
  let rec cmp = (l, r) => {
    switch (l, r) {
    | ([], []) => 0
    | ([], _) => (-1)
    | (_, []) => 1
    | ([lh, ...lt], [rh, ...rt]) =>
      switch (RangeSet.compare(lh, rh)) {
      | 0 => cmp(lt, rt)
      | n => n
      }
    };
  };
  cmp(l, r);
};

let of_string = (~allow_overlap=true, s) => {
  List.map(
    c => RangeSet.singleton(Range.singleton(~allow_overlap, c, c)),
    Common.explode(s),
  );
};

let choose_strict: t => RangeSet.t =
  r =>
    switch (List.length(r)) {
    | 1 => List.hd(r)
    | 0 => raise(NoElement("Expected 1 element, got 0"))
    | n =>
      raise(
        MultipleElements("Expected 1 element, got " ++ string_of_int(n)),
      )
    };

let test = () => {
  let r1 = of_string("abc");
  let r2 = of_string("ab");
  let r2 = r2 @ [RangeSet.singleton(Range.singleton('b', 'd'))];
  assert(to_string(r1) == "abc");
  assert(to_string(r2) == "ab[b-d]");
  assert(String.concat("", generate_strings(r2)) == "abbabcabd");
};