type t = (char, char);

exception InvalidRange(string);
exception RangeOverlap(string);

let to_string: t => string =
  r =>
    switch (r) {
    | (from_char, to_char) =>
      switch (Char.compare(from_char, to_char)) {
      | 0 => String.make(1, from_char)
      | _ => String.make(1, from_char) ++ "-" ++ String.make(1, to_char)
      }
    };

let to_char_set: t => CharSet.t =
  r => {
    let rec aux = ((from_char, to_char), char_set) => {
      let from_char_code = Char.code(from_char);
      let to_char_code = Char.code(to_char);
      assert(from_char_code <= to_char_code);
      let char_set = CharSet.add(from_char, char_set);
      if (from_char_code == to_char_code) {
        char_set;
      } else {
        aux((Char.chr(succ(from_char_code)), to_char), char_set);
      };
    };
    aux(r, CharSet.empty);
  };

let singleton: (char, char) => t =
  (from_char, to_char) =>
    if (Char.code(from_char) > Char.code(to_char)) {
      raise(InvalidRange(to_string((from_char, to_char))));
    } else {
      (from_char, to_char);
    };

/** overlaps iif "not (end1 < start2 or end2 < start1)"
    taken from http://wiki.c2.com/?TestIfDateRangesOverlap */
let compare = ((from1, to1), (from2, to2)) =>
  switch (Char.compare(to1, from2), Char.compare(to2, from1)) {
  | (cmp1, cmp2) when cmp1 < 0 || cmp2 < 0 =>
    /* ranges do not overlap */
    switch (Char.compare(from1, from2)) {
    | 0 => Char.compare(to1, to2)
    | n => n
    }
  | (_, _) =>
    switch (Char.compare(from1, from2), Char.compare(to1, to2)) {
    | (0, 0) => 0 /* ranges identical */
    | (_, _) =>
      raise(
        RangeOverlap(
          to_string((from1, to1)) ++ " overlaps " ++ to_string((from2, to2)),
        ),
      )
    }
  };

let test = () => {
  let ac = singleton('a', 'c');
  let df = singleton('d', 'f');
  assert(compare(ac, ac) == 0);
  assert(compare(ac, df) < 0);
  assert(compare(df, ac) > 0);
};