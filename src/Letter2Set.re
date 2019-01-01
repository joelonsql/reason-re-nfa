module Pair = {
  type t = (Letter.t, Letter.t);

  let compare = (((_, w), (_, x)), ((_, y), (_, z))) =>
    switch (Int32.compare(w, y)) {
    | 0 => Int32.compare(x, z)
    | i => i
    };
};

include Set.Make(Pair);

let (<+>) = union;

/** Flat map */

let (>>=) = (m, k) => LetterSet.fold((x, s) => union(k(x), s), m, empty);

/** Cartensian product, i.e. all elements in l combined with all elements in r */

let (<*>): (LetterSet.t, LetterSet.t) => t =
  (l, r) => l >>= (x => r >>= (y => singleton((x, y))));

let to_string = l2s =>
  String.concat(
    " ",
    List.rev(
      fold(
        ((letter1, letter2), l) =>
          [Letter.to_string(letter1) ++ Letter.to_string(letter2), ...l],
        l2s,
        [],
      ),
    ),
  );

let example = (letter1, letter2) => singleton((letter1, letter2));

let test = () => {
  let abc =
    example(Letter.example(['a'], 0), Letter.example(['b', 'c'], 1));
  assert(to_string(abc) == "a<sub>0</sub>[bc]<sub>1</sub>");
  let a = LetterSet.example(['a'], 0);
  let bc = LetterSet.example(['b', 'c'], 1);
  let abc' = a <*> bc;
  assert(equal(abc, abc'));
  let abc'' = a >>= (x => bc >>= (y => singleton((x, y))));
  assert(equal(abc, abc''));
};
