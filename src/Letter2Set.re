module Pair = {
  type t = (Letter.t, Letter.t);
  let compare = (((_, w), (_, x)), ((_, y), (_, z))) =>
    Pervasives.compare((w, x), (y, z));
};

module S = Set.Make(Pair);

let (<+>) = S.union;

/** Flat map */
let (>>=) = (m, k) => LetterSet.S.fold(
  (x, s) => k(x) <+> s,
  m,
  S.empty
);

/** Cartensian product, i.e. all elements in l combined with all elements in r */
let (<*>): (LetterSet.S.t, LetterSet.S.t) => S.t =
  (l, r) =>
    l >>= (x =>
      r >>= (y =>
        S.singleton((x, y))
      )
    );

let to_string = (l2s) => String.concat(
  " ",
  List.rev(S.fold(
    ((letter1, letter2), l) => [Letter.to_string(letter1) ++ Letter.to_string(letter2), ...l],
    l2s,
    [],
  ))
);

let example = (letter1, letter2) => S.singleton((letter1, letter2));

let test = () => {
  let abc = example(Letter.example(['a'],0), Letter.example(['b','c'],1));
  assert(to_string(abc) == "a<sub>0</sub>{b c}<sub>1</sub>");
  let a = LetterSet.example(['a'],0);
  let bc = LetterSet.example(['b','c'],1);
  let abc' = a <*> bc;
  assert(abc == abc');
  let abc'' = a >>= (x => bc >>= (y => S.singleton((x, y))));
  assert(abc == abc'');
};
