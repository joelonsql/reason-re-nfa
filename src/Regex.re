type regex('c) =
  | Empty: regex('c)
  | Eps: regex('c)
  | Char('c): regex('c)
  | Alt(regex('c), regex('c)): regex('c)
  | Seq(regex('c), regex('c)): regex('c)
  | Star(regex('c)): regex('c);

type t = regex(RangeSet.t);
type charset = RangeSet.t;

/** Various basic and derived regex combinators */

let seq = (l, r) =>
  switch (l, r) {
  | (Eps, s)
  | (s, Eps) => s
  | (l, r) => Seq(l, r)
  };

let alt = (l, r) =>
  switch (l, r) {
  | (Char(c1), Char(c2)) => Char(RangeSet.union(c1, c2))
  | (l, r) => Alt(l, r)
  };

let star = r => Star(r);

let plus = t => seq(t, star(t));

let eps = Eps;

let chr = c => Char(RangeSet.singleton(Range.singleton(c, c)));

let opt = t => alt(t, eps);

let empty = Empty;

let range_ = (l, h) => {
  RangeSet.singleton(Range.singleton(l, h));
};

let range = (l, h) => Char(range_(l, h));

let any_ = range_(Char.chr(1), Char.chr(255));

let any = Char(any_);