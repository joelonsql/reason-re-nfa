type t = list(RangeSet.t);

exception NoElement(string);
exception MultipleElements(string);

let compare = (l, r) => {
  let rec cmp = (l, r) =>
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
  cmp(l, r);
};

let to_string: t => string =
  range_set_list => {
    String.concat("", List.map(RangeSet.to_string, range_set_list));
  };

let of_string = (~allow_overlap=false, s) => {
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
  print_endline(to_string(r1));
  print_endline(to_string(r2));
};