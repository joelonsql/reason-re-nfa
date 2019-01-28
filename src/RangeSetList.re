type t = list(RangeSet.t);

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

let of_string: string => t =
  s => {
    List.map(
      c => RangeSet.singleton(Range.singleton(c, c)),
      Common.explode(s),
    );
  };

let test = () => {
  let r1 = of_string("abc");
  let r2 = of_string("ab");
  let r2 = r2 @ [RangeSet.singleton(Range.singleton('b', 'd'))];
  print_endline(to_string(r1));
  print_endline(to_string(r2));
};