include Set.Make(Char);

/** range_extract and string_of_range taken from
    https://www.rosettacode.org/wiki/Range_extraction#OCaml */

let range_extract =
  fun
  | [] => []
  | [x, ...xs] => {
      let f = ((i, j, ret), k) =>
        if (k == succ(j)) {
          (i, k, ret);
        } else {
          (k, k, [(i, j), ...ret]);
        };

      let (m, n, ret) = List.fold_left(f, (x, x, []), xs);
      List.rev([(m, n), ...ret]);
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

let to_string = char_set =>
  switch (cardinal(char_set)) {
  | 0 => ""
  | 1 => Common.escaped(choose(char_set))
  | 256 => "."
  | _ =>
    "["
    ++ string_of_range(
         range_extract(List.map(Char.code, elements(char_set))),
       )
    ++ "]"
  };

let example = char_list => of_list(char_list);

let test = () => {
  assert(to_string(example([])) == "");
  assert(to_string(example(['a'])) == "a");
  assert(to_string(example(['"'])) == "\\\"");
  assert(to_string(example([Char.chr(1)])) == "\\\\x01");
  assert(to_string(example(['b', 'a', 'c'])) == "[a-c]");
};