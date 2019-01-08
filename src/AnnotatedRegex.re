open Regex;

type t('a) = regex(('a, int32));

/** Give every character set in 'r' a unique identifier */

let annotate: 'a. regex('a) => t('a) =
  r => {
    let rec annotate: 'a. (int32, regex('a)) => (int32, t('a)) =
      count =>
        fun
        | Empty => (count, Empty)
        | Eps => (count, Eps)
        | Char(c) => {
            let count = Int32.succ(count);
            let p = (c, count);
            (count, Char(p));
          }
        | Alt(e, f) => {
            let (count', e') = annotate(count, e);
            let (count'', f') = annotate(count', f);
            (count'', Alt(e', f'));
          }
        | Seq(e, f) => {
            let (count', e') = annotate(count, e);
            let (count'', f') = annotate(count', f);
            (count'', Seq(e', f'));
          }
        | Star(e) => {
            let (count', e') = annotate(count, e);
            (count', Star(e'));
          };

    let (_, annotated) = annotate(Int32.zero, r);
    annotated;
  };

let rec to_string = annotated =>
  switch (annotated) {
  | Empty => ""
  | Eps => "&epsilon;"
  | Star(x) => "(" ++ to_string(x) ++ ")*"
  | Seq(a, b) => to_string(a) ++ to_string(b)
  | Alt(a, b) => "(" ++ to_string(a) ++ "|" ++ to_string(b) ++ ")"
  | Char(x) =>
    switch (x) {
    | (c, i) =>
      CharSet.to_string(c) ++ "<sub>" ++ Int32.to_string(i) ++ "</sub>"
    }
  };
