open Regex;

/** Follows the POSIX spec
        9.3.5 RE Bracket Expression
          http://pubs.opengroup.org/onlinepubs/009696899/basedefs/xbd_chap09.html#tag_09_03_05
      but there is currently no support for character classes.

      Bracket expressions are delimited by [ and ], with an optional "complement"
      operator ^ immediately after the [.  Special characters:
          ^ (immediately after the opening [)
          ] (except immediately after the opening [ or [^)
          - (except at the beginning or end)                                   */;

exception Fail;

type element =
  | Char(char)
  | Range(char, char);
type t = {
  negated: bool,
  elements: list(element),
};

let interpret = ({negated, elements}) => {
  let s =
    List.fold_right(
      fun
      | Char(c) => CharSet.add(c)
      | Range(c1, c2) => CharSet.union(range_(c1, c2)),
      elements,
      CharSet.empty,
    );
  if (negated) {
    CharSet.diff(any_, s);
  } else {
    s;
  };
};

let parse_element =
  fun
  | [] => raise(Fail)
  | [']', ...s] => (None, s)
  | [c, ...['-', ']', ..._] as s] => (Some(Char(c)), s)
  | [c1, '-', c2, ...s] => (Some(Range(c1, c2)), s)
  | [c, ...s] => (Some(Char(c)), s);

let parse_initial =
  fun
  | [] => raise(Fail)
  | [c, ...['-', ']', ..._] as s] => (Some(Char(c)), s)
  | [c1, '-', c2, ...s] => (Some(Range(c1, c2)), s)
  | [c, ...s] => (Some(Char(c)), s);

let parse_elements = s => {
  let rec loop = (elements, s) =>
    switch (parse_element(s)) {
    | (None, s) => (List.rev(elements), s)
    | (Some(e), s) => loop([e, ...elements], s)
    };
  switch (parse_initial(s)) {
  | (None, s) => ([], s)
  | (Some(e), s) => loop([e], s)
  };
};

type result = {
  hyphen: bool,
  caret: bool,
  lbracket: bool,
  ranges: list((char, char)),
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
            {...r, ranges: [(c1, c2), ...r.ranges]},
          )
        },
      set,
      (None, {hyphen: false, caret: false, lbracket: false, ranges: []}),
    )
  ) {
  | (None, r) => {...r, ranges: List.rev(r.ranges)}
  | (Some(p), r) => {...r, ranges: List.rev([p, ...r.ranges])}
  };
};

let regex_specials = "*+?.|()[";

let unparse = (~complement=false, set: CharSet.t) => {
  let pr = Printf.sprintf;
  let r = ranges(set);
  let conc =
    List.fold_left(
      (s, (x, y)) =>
        if (x == y) {
          pr("%c%s", x, s);
        } else {
          pr("%c-%c%s", x, y, s);
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
  switch (r.ranges, r.lbracket, r.caret, r.hyphen) {
  | ([(x, y)], false, false, false)
      /* If we have a single non-special character then
         there's no need for a range expression */
      when x == y && !complement && !String.contains(regex_specials, x) =>
    pr("%c", x)
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