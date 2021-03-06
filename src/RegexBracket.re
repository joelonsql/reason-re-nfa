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
      | Char(c) => RangeSet.add(Range.singleton(~allow_overlap=true, c, c))
      | Range(c1, c2) => RangeSet.union(range_(c1, c2)),
      elements,
      RangeSet.empty,
    );
  if (negated) {
    RangeSet.diff(any_, s);
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