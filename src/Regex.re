/** Convert a regex to an ε-free NFA using a slight modification of
    Glushkov's algorithm.

    (The modification: we label character sets rather than characters
     to prevent a state explosion.)
 */

type regex('c) =
  | Empty: regex('c) /* L = { } */
  | Eps: regex('c) /* L = {ε} */
  | Char('c): regex('c)
  | Alt(regex('c), regex('c)): regex('c)
  | Seq(regex('c), regex('c)): regex('c)
  | Star(regex('c)): regex('c);

type t = regex(CharSet.t);

/** Various basic and derived regex combinators */

let seq = (l, r) =>
  switch (l, r) {
  | (Eps, s)
  | (s, Eps) => s
  | (l, r) => Seq(l, r)
  };
let alt = (l, r) =>
  switch (l, r) {
  | (Char(c1), Char(c2)) => Char(CharSet.union(c1, c2))
  | (l, r) => Alt(l, r)
  };
let star = r => Star(r);
let plus = t => seq(t, star(t));
let eps = Eps;
let chr = c => Char(CharSet.singleton(c));
let opt = t => alt(t, eps);
let empty = Empty;

let range_ = (l, h) => {
  let rec loop = (i, h, acc) =>
    if (i == h) {
      CharSet.add(Char.chr(i), acc);
    } else {
      loop(succ(i), h, CharSet.add(Char.chr(i), acc));
    };
  loop(Char.code(l), Char.code(h), CharSet.empty);
};

let range = (l, h) => Char(range_(l, h));
let any = range(Char.chr(0), Char.chr(255));
