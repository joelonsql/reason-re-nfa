type t = {
  from_char: char,
  to_char: char,
  allow_overlap: bool,
};

exception InvalidRange(string);
exception RangeOverlap(string);

let to_string: t => string =
  r =>
    Common.escape_string(
      switch (Char.compare(r.from_char, r.to_char)) {
      | 0 => String.make(1, r.from_char)
      | _ => String.make(1, r.from_char) ++ "-" ++ String.make(1, r.to_char)
      },
    );

let to_char_set: t => CharSet.t =
  r => {
    let rec aux = (r, char_set) => {
      let from_char_code = Char.code(r.from_char);
      let to_char_code = Char.code(r.to_char);
      assert(from_char_code <= to_char_code);
      let char_set = CharSet.add(r.from_char, char_set);
      if (from_char_code == to_char_code) {
        char_set;
      } else {
        let from_char = Char.chr(succ(from_char_code));
        aux({...r, from_char}, char_set);
      };
    };
    aux(r, CharSet.empty);
  };

let singleton = (~allow_overlap=false, from_char, to_char) =>
  if (Char.code(from_char) > Char.code(to_char)) {
    raise(InvalidRange(to_string({from_char, to_char, allow_overlap})));
  } else {
    {from_char, to_char, allow_overlap};
  };

/** overlaps iif "not (end1 < start2 or end2 < start1)"
    taken from http://wiki.c2.com/?TestIfDateRangesOverlap */
let compare = (range1, range2) =>
  switch (
    Char.compare(range1.to_char, range2.from_char),
    Char.compare(range2.to_char, range1.from_char),
  ) {
  | (cmp1, cmp2) when cmp1 < 0 || cmp2 < 0 =>
    /* ranges do not overlap */
    switch (Char.compare(range1.from_char, range2.from_char)) {
    | 0 => Char.compare(range1.to_char, range2.to_char)
    | n => n
    }
  | (_, _) =>
    switch (
      Char.compare(range1.from_char, range2.from_char),
      Char.compare(range1.to_char, range2.to_char),
      range1.allow_overlap && range2.allow_overlap,
    ) {
    | (0, 0, _) => 0 /* ranges identical */
    | (0, cmp_to, true) => cmp_to
    | (cmp_from, _, true) => cmp_from
    | (_, _, false) =>
      raise(
        RangeOverlap(
          to_string(range1) ++ " overlaps " ++ to_string(range2),
        ),
      )
    }
  };

let test = () => {
  let ac = singleton(~allow_overlap=true, 'a', 'c');
  let bd = singleton(~allow_overlap=true, 'b', 'd');
  assert(compare(ac, ac) == 0);
  assert(compare(ac, bd) < 1);
  let ac = singleton('a', 'c');
  let eg = singleton('e', 'g');
  assert(compare(ac, ac) == 0);
  assert(compare(ac, eg) < 0);
  assert(compare(eg, ac) > 0);
};