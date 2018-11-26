open Regex;

type t('a) =
  | One('a, t('a))
  | Two('a, t('a), t('a))
  | Leaf('a);

let rec of_regex = (r) => switch(r) {
  | Empty => Leaf("Empty")
  | Eps => Leaf("Eps")
  | Star(s) => One("Star", of_regex(s))
  | Seq(l, r) => Two("Seq", of_regex(l), of_regex(r))
  | Alt(l, r) => Two("Alt", of_regex(l), of_regex(r))
  | Char(s) => One("Char", Leaf(switch (CharSet.S.cardinal(s)) {
    | 0 => "{}"
    | 1 => String.make(1, CharSet.S.choose(s))
    | 256 => "."
    | _ => "{" ++ String.concat(" ", List.map(String.make(1), CharSet.S.elements(s))) ++ "}"
  }))
};
