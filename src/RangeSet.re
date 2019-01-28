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

let to_string = range_set =>
  "["
  ++ String.concat("", List.map(Range.to_string, elements(range_set)))
  ++ "]";

let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [of_char(s.[i]), ...l]);
    };
  exp(String.length(s) - 1, []);
};

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
  print_endline(str);
  assert(str == "[a-cde-g]");
  let char_set = CharSet.of_list(['e', 'f', 'c', 'a', 'b', 'h']);
  let range_set = of_char_set(char_set);
  let range_set = add(Range.singleton('j', 'm'), range_set);
  let str = to_string(range_set);
  assert(str == "[a-ce-fhj-m]");
  let range_set = add(Range.singleton('d', 'd'), range_set);
  let str = to_string(range_set);
  print_endline(str);
  let char_set = to_char_set(range_set);
  print_endline(CharSet.to_string(char_set));
  let range_set = of_char_set(char_set);
  print_endline(to_string(range_set));
};