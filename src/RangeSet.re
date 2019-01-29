include Set.Make(Range);

/** range_extract and string_of_range taken from
    https://www.rosettacode.org/wiki/Range_extraction#OCaml */
let range_extract = (~allow_overlap=false) =>
  fun
  | [] => []
  | [x, ...xs] => {
      let f = ((i, j, ret), k) =>
        if (Char.code(k) == succ(Char.code(j))) {
          (i, k, ret);
        } else {
          (k, k, [{Range.from_char: i, to_char: j, allow_overlap}, ...ret]);
        };

      let (m, n, ret) = List.fold_left(f, (x, x, []), xs);
      List.rev([
        {Range.from_char: m, Range.to_char: n, allow_overlap},
        ...ret,
      ]);
    };

let string_of_range = rng => {
  let str = ((a, b)) =>
    if (a == b) {
      Common.escaped(Char.chr(a));
    } else {
      Printf.sprintf(
        "%s-%s",
        Common.escaped(Char.chr(a)),
        Common.escaped(Char.chr(b)),
      );
    };

  String.concat("", List.map(str, rng));
};

let of_char = (~allow_overlap=false, char) =>
  singleton(Range.singleton(~allow_overlap, char, char));

let of_char_set = (~allow_overlap=false, char_set) =>
  List.fold_right(
    (range, range_set) => add(range, range_set),
    range_extract(~allow_overlap, CharSet.elements(char_set)),
    empty,
  );

let to_char_set: t => CharSet.t =
  range_set =>
    List.fold_right(
      (range, char_set) =>
        CharSet.union(Range.to_char_set(range), char_set),
      elements(range_set),
      CharSet.empty,
    );

let diff = (r1, r2) => {
  of_char_set(CharSet.diff(to_char_set(r1), to_char_set(r2)));
};

let inter = (r1, r2) => {
  of_char_set(CharSet.inter(to_char_set(r1), to_char_set(r2)));
};

type result = {
  hyphen: bool,
  caret: bool,
  lbracket: bool,
  ranges: t,
};
let ranges = (set: CharSet.t) => {
  let adjacent = (c1, c2) => abs(Char.code(c1) - Char.code(c2)) == 1;
  switch (
    CharSet.fold(
      (c, (co, r)) =>
        switch (c, co) {
        | ('-', co) => (co, {...r, hyphen: true})
        | ('^', co) => (co, {...r, caret: true})
        | (']', co) => (co, {...r, lbracket: true})
        | (c, None) => (Some((c, c)), r)
        | (c, Some((c1, c2))) when adjacent(c, c2) => (Some((c1, c)), r)
        | (c, Some((c1, c2))) => (
            Some((c, c)),
            {
              ...r,
              ranges:
                add(Range.singleton(~allow_overlap=true, c1, c2), r.ranges),
            },
          )
        },
      set,
      (None, {hyphen: false, caret: false, lbracket: false, ranges: empty}),
    )
  ) {
  | (None, r) => {...r, ranges: r.ranges}
  | (Some((c1, c2)), r) => {
      ...r,
      ranges: add(Range.singleton(~allow_overlap=true, c1, c2), r.ranges),
    }
  };
};

let unparse = (~complement=false, set: CharSet.t) => {
  let regex_specials = "*+?.|()[";
  let pr = Printf.sprintf;
  let r = ranges(set);
  let conc =
    List.fold_left(
      (s, range) =>
        if (range.Range.from_char == range.to_char) {
          pr("%c%s", range.from_char, s);
        } else {
          pr("%c-%c%s", range.from_char, range.to_char, s);
        },
      "",
    );
  let whenever = (p, s) => if (p) {s} else {""};
  let bracket = s =>
    if (complement) {
      pr("[^%s]", s);
    } else {
      pr("[%s]", s);
    };
  switch (List.rev(elements(r.ranges)), r.lbracket, r.caret, r.hyphen) {
  | ([range], false, false, false)
      /* If we have a single non-special character then
         there's no need for a range expression */
      when
        Char.compare(range.from_char, range.to_char) == 0
        && !complement
        && !String.contains(regex_specials, range.from_char) =>
    pr("%c", range.from_char)
  | ([_, ..._] as rs, lbracket, caret, hyphen) =>
    /* If we have some non-special characters then we don't need to
       take extra care to avoid accidentally positioning special
       characters like ^ the beginning or end */
    bracket @@
    pr(
      "%s%s%s%s",
      whenever(lbracket, "]"),
      conc(rs),
      whenever(caret, "^"),
      whenever(hyphen, "-"),
    )
  | ([], true, _, _) =>
    bracket(pr("]%s%s", whenever(r.caret, "^"), whenever(r.hyphen, "-")))
  | ([], false, true, true) => bracket("-^")
  | ([], false, true, false) => if (complement) {"[^^]"} else {"^"}
  | ([], false, false, true) => bracket("-")
  | ([], false, false, false) =>
    pr(
      "[%s%c-%c]",
      if (complement) {""} else {"^"},
      Char.chr(1),
      Char.chr(255),
    )
  };
};

let unparse_charset = s => {
  let pos = unparse(~complement=false, s)
  and neg =
    unparse(
      ~complement=true,
      CharSet.diff(
        Range.to_char_set(
          Range.singleton(~allow_overlap=true, Char.chr(1), Char.chr(255)),
        ),
        s,
      ),
    );
  if (String.length(pos) <= String.length(neg)) {
    pos;
  } else {
    neg;
  };
};
let to_string = range_set =>
  Common.escape_string(unparse_charset(to_char_set(range_set)));

let set_allow_overlap = (allow_overlap, range_set) => {
  List.fold_right(
    (range, range_set) => add({...range, allow_overlap}, range_set),
    elements(range_set),
    empty,
  );
};

let test = () => {
  let range_set = singleton(Range.singleton('a', 'c'));
  let range_set = add(Range.singleton('e', 'g'), range_set);
  let range_set = add(Range.singleton('d', 'd'), range_set);
  let str = to_string(range_set);
  assert(str == "[a-g]");
  let char_set = CharSet.of_list(['e', 'f', 'c', 'a', 'b', 'h']);
  let range_set = of_char_set(char_set);
  let range_set = add(Range.singleton('j', 'm'), range_set);
  let str = to_string(range_set);
  assert(str == "[a-ce-fhj-m]");
  let range_set = add(Range.singleton('d', 'd'), range_set);
  let str = to_string(range_set);
  assert(str == "[a-fhj-m]");
  let char_set = to_char_set(range_set);
  let range_set = of_char_set(char_set);
  let str = to_string(range_set);
  assert(str == "[a-fhj-m]");
};